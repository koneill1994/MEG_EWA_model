# EWA Model class

library(dplyr)
library(truncnorm)
library(ggplot2)
library(foreach)
library(doParallel)


Make_EWA_model <- setRefClass(
  "EWA_Model",
  fields = list(
    choices         = "integer",
    attraction      = "numeric",
    attraction_prev = "numeric",
    N               = "numeric",
    N_prev          = "numeric",
    delta           = "numeric",
    rho             = "numeric",
    lambda          = "numeric",
    phi             = "numeric",
    choice_prob     = "numeric",
    own_choice      = "numeric",
    id              = "character",
    get_payoff      = "function"
  ),
  methods=list(
    # use <<- because non-local variable assignment
    # p_mean, p_sd : parameter means, parameter sd's
    # [delta, rho, lambda, phi]
    initialize=function(id,p_mean, p_sd, initial_choice_prob, payoff_function){
      id                <<- id
      choices           <<- 1:7
      attraction        <<- rep(0,7)
      attraction_prev   <<- rep(0,7)
      N                 <<- rep(  0, length(choices))
      N_prev            <<- rep(  0, length(choices))
      delta             <<- rtruncnorm(1, a=0, b=1, mean = p_mean[1], sd = p_sd[1]  ) 
      rho               <<- rtruncnorm(1, a=0, b=1, mean = p_mean[2], sd = p_sd[2]  ) 
      lambda            <<- rtruncnorm(1, a=0, b=1, mean = p_mean[3], sd = p_sd[3]  ) 
      phi               <<- rtruncnorm(1, a=0, b=1, mean = p_mean[4], sd = p_sd[4]  ) 
      choice_prob       <<- initial_choice_prob
      own_choice        <<- NaN # set it to something invalid initially for sanity testing
      get_payoff        <<- payoff_function;
    },
    update=function(group_choices){

      # run through each (unique) player's choice
      # update payoffs assuming that player's choice is the minimum
      delta_n = ifelse(choices == own_choice, 1, delta )
      
      payoff = get_payoff(choices, own_choice, group_choices)
      
      weighted_payoff = delta_n*payoff
      
      N               <<- rho*N_prev+1
      attraction      <<- (phi*N_prev*attraction_prev + weighted_payoff)/N
      
      
      if(sum(exp(lambda * attraction)) == Inf){
        # convenience code to catch examples that would be coerced to Inf
        #   i don't like it either but we have to run this on finite computers
        #   not in infinite abstract mathematical space
        choice_prob<<-ifelse(exp(lambda * attraction)==Inf,1,0)
      } else if(sum(exp(lambda * attraction)) == 0){
        # if somehow all the attractions are so far negative that they
        #   all exponentiate as -Inf, just make choices randomly
        choice_prob   <<- rep(1/length(attraction),length(attraction))
      } else{
        # normal EWA operation
        choice_prob   <<- exp(lambda * attraction) / sum( exp(lambda * attraction) )
      }
      attraction_prev <<- attraction
      N_prev          <<- N
    
    },
    choose= function(){
      own_choice        <<- sample(choices, 1,replace=T, prob=choice_prob)
      return(own_choice)
    },
    #function to output state to a datafile
    output_logfile=function(){
      # attractions, N value, delta, rho, lambda, phi, choice probabilities
      
      # this I(list(x)) tomfoolery is so I can put lists into dataframe cells
      # without it converting them to vertical columns
      # for reference
      # https://www.r-bloggers.com/populating-data-frame-cells-with-more-than-one-value/
      
      return(
        data.frame( own_choice=own_choice, 
                    attraction=I(list(attraction)),
                    N=I(list(N)),
                    choice_prob=I(list(choice_prob)),
                    delta=delta, 
                    rho=rho, 
                    lambda=lambda, 
                    phi=phi
        )
      )
    }
  )
)


model_run=function(parameter_means, parameter_sds, choice_data, n_sims, h_dat, payoff_f){
  # run the model n_sims times to get a modelled dataset based on 
  # inputted parameters
  model_data_full=data.frame()
  start_time=Sys.time()
  
  for(sim in 1:n_sims){
    # so we don't spam the console
    if(sim%%10==0 & verbose){
      print(paste("sim ",sim))
    }
    model_list=c(Make_EWA_model("1",parameter_means,parameter_sds,choice_data, payoff_f),
                 Make_EWA_model("2",parameter_means,parameter_sds,choice_data, payoff_f),
                 Make_EWA_model("3",parameter_means,parameter_sds,choice_data, payoff_f),
                 Make_EWA_model("4",parameter_means,parameter_sds,choice_data, payoff_f))
    
    for(round in 1:20){
      g_choices=c()
      # all models make their choice for this round
      for(model in model_list){
        g_choices=c(g_choices,model$choose())
      }
      # all models update internal state based on choices made this round
      for(model in model_list){
        model$update(g_choices)
        
        # log data
        header=data.frame(sim=sim, model=model$id, round=round)
        m_log=cbind(header,model$output_logfile())
        model_data_full=rbind(model_data_full, m_log)
      }
    }
  }
  # h_dat is the corresponding dataframe to model_data
  # we're going to combine them below
  model_data=model_data_full[,1:4]
  
  # df columns:
  # agent_type, round, mean, se
  
  # average across groups and players
  # to get average choice made for each round for humans and models
  # this dataframe combines h_dat and model_data
  compare_dat=rbind(
    data.frame(unique(select(mutate(group_by(model_data, round),
                                    mean=mean(own_choice),
                                    agent_type="model"
    ),
    agent_type,round,mean))
    ),
    data.frame(unique(select(mutate(group_by(h_dat, round),
                                    mean=mean(choice),
                                    agent_type="human"
    ),
    agent_type,round,mean))
    )
  )

  return(
    data.frame( delta=mean(model_data_full$delta),
                rho=mean(model_data_full$rho),
                lambda=mean(model_data_full$lambda),
                phi=mean(model_data_full$phi),
                initial_choice_data=I(list(choice_prob_data)),
                rmse=sqrt(mean(( compare_dat[compare_dat$agent_type=="human",]$mean-
                                   compare_dat[compare_dat$agent_type=="model",]$mean)^2)),
                computation_time=Sys.time()-start_time
    )
  )
}



drawgraphs=F
load_human_dat=T
verbose=T

setwd("C:/Users/Kevin/Dropbox/minimum_effort_game/EWA_model")

# get human data to compare models to
if(!load_human_dat){
  source("Data_Analysis.R")
  # this should give us JUST d1a as a dataframe object
  
  # game, subject, round, choice
  human_data=data.frame(group=d1a$Group, subject=d1a$Subject_ID, round=d1a$Round, choice=d1a$Effort_level)
  # this is the data we're going to compare things to
  saveRDS(human_data, file="MEG_human_data.rds")
} else{
  # load in the human data we've prepared earlier
  human_data=readRDS("MEG_human_data.rds")
}

# these are the defaults, we're going to iterate over different values for these
# p_m, psd : parameter means, parameter sd's
# [delta, rho, lambda, phi]
p_m=c(1,1,1,1)
psd=c(.01,.01,.01,.01)

# initial choice probabilities based on human data
choice_prob_data=c(0.025, 0.100, 0.200, 0.250, 0.175, 0.075, 0.175)

# number of simulations to run
number_of_sims=100

payoff_f = function(choices,own_choice, group_choices){
  g_min = min(group_choices)
  max_payoff =  ((g_min - 1) * 10) + 70
  payoff = ifelse( choices < g_min, ((choices - 1) * 10) + 70, max_payoff - (choices-g_min)*10 )
  return(payoff)
}


# loop through different parameter sets here
param_means= (0:1)/10

registerDoParallel()

getDoParWorkers()


fit_data= foreach(d=param_means, .combine=rbind) %:%
  foreach(rh=param_means, .combine=rbind) %:%
  foreach(l=param_means, .combine=rbind) %:%
  foreach(ph=param_means, .combine=rbind) %do% {
    pm=c(d,rh,l,ph)
    if(verbose) print(pm)
    model_run(pm, psd, choice_prob_data, number_of_sims, human_data, payoff_f)
  }



saveRDS(fit_data, file=paste(format(Sys.time(),"%Y-%m-%d_%H-%M-%S"),"_EWA-fit.rds",sep=""))
