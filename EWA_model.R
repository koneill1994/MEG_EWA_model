# EWA Model class

library(dplyr)
library(truncnorm)
library(ggplot2)

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
    id              = "character"
    ),
  methods=list(
    # use <<- because non-local variable assignment
    # p_mean, p_sd : parameter means, parameter sd's
    # [delta, rho, lambda, phi]
    initialize=function(id,p_mean, p_sd, initial_choice_prob){
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
    },
    update=function(group_choices){
      cl_0=unique(group_choices)
      choice_list=sample(cl_0,length(cl_0))
      
      # run through each (unique) player's choice
      # update payoffs assuming that player's choice is the minimum
      for(g_min in choice_list){
        delta_n = ifelse(choices == own_choice, 1, delta )
        max_payoff =  ((g_min - 1) * 10) + 70
        
        payoff = ifelse( choices < g_min, ((choices - 1) * 10) + 70, max_payoff - (choices-g_min)*10 )
        
        weighted_payoff = delta_n*payoff
        
        N               <<- rho*N_prev+1
        attraction      <<- (phi*N_prev*attraction_prev + weighted_payoff)/N
        choice_prob     <<- exp(lambda * attraction) / sum( exp(lambda * attraction) )
        
        attraction_prev <<- attraction
        N_prev          <<- N
      }
    },
    choose= function(){
      # this is where it is throwing an error
      # its happening because somehow choice_prob contains an NA
      # how to avoid this?
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


model_run=function(parameter_means, parameter_sds, choice_data, n_sims, h_dat){
  # run the model n_sims times to get a modelled dataset based on 
  # inputted parameters
  model_data_full=data.frame()
  
  for(sim in 1:n_sims){
    # so we don't spam the console
    if(sim%%10==0){
      print(paste("sim ",sim))
    }
    
    model_list=c(Make_EWA_model("1",parameter_means,parameter_sds,choice_data),
                 Make_EWA_model("2",parameter_means,parameter_sds,choice_data),
                 Make_EWA_model("3",parameter_means,parameter_sds,choice_data),
                 Make_EWA_model("4",parameter_means,parameter_sds,choice_data))
    
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
                                    se=sd(own_choice)/sqrt(length(own_choice)),
                                    agent_type="model"
                                    ),
                             agent_type,round,mean,se))
               ),
    data.frame(unique(select(mutate(group_by(h_dat, round),
                                    mean=mean(choice),
                                    se=sd(choice)/sqrt(length(choice)),
                                    agent_type="human"
                                    ),
                             agent_type,round,mean,se))
               )
  )
  # reminder that for a full parameter space search
  # SE is not necessary and a pruned version of this function should be used
  
  
  # if you really want to look at an individual parameter set
  # and its fit to human data
  if(drawgraphs){
    # look at it visually
    ggplot(compare_dat)+
      geom_line(aes(x=round, y=mean, color=agent_type))+
      geom_errorbar(mapping=aes(x=round, ymin=mean-se, ymax=mean+se, color=agent_type), 
                    width=.2)+
      labs("average choice")+
      labs(title="average choice by round for human and model agents",
           caption="error bars represent standard error")
  }
  
  # dataframe to hold info about parameters and model fit
  # delta, rho, lambda, phi, initial_choice_data, rmse
  return(
    data.frame( delta=mean(model_data_full$delta),
                rho=mean(model_data_full$rho),
                lambda=mean(model_data_full$lambda),
                phi=mean(model_data_full$phi),
                initial_choice_data=I(list(choice_prob_data)),
                rmse=sqrt(mean(( compare_dat[compare_dat$agent_type=="human",]$mean-
                                   compare_dat[compare_dat$agent_type=="model",]$mean)^2))
         )
  )
}


setwd("C:/Users/Kevin/Dropbox/minimum_effort_game/EWA_model")
source("Data_Analysis.R")
# this should give us JUST d1a as a dataframe object

# game, subject, round, choice
human_data=data.frame(group=d1a$Group, subject=d1a$Subject_ID, round=d1a$Round, choice=d1a$Effort_level)
# this is the data we're going to compare things to


drawgraphs=F

# these are the defaults, we're going to iterate over different values for these
# p_m, psd : parameter means, parameter sd's
# [delta, rho, lambda, phi]
p_m=c(1,1,1,1)
psd=c(.01,.01,.01,.01)

# initial choice probabilities based on human data
choice_prob_data=c(0.025, 0.100, 0.200, 0.250, 0.175, 0.075, 0.175)

# number of simulations to run
number_of_sims=100

# initialize fit_data
fit_data=data.frame()

# loop through different parameter sets here
param_means= (0:9)/10

for(d in param_means){
  for(rh in param_means){
    for(l in param_means){
      for(ph in param_means){
        pm=c(d,rh,l,ph)
        print(pm)
        fit_data=rbind(fit_data,
                       model_run(pm, psd, choice_prob_data, number_of_sims, human_data)
        )
      }
    }
  }
}
  
View(fit_data)

save(fit_data, file=paste(format(Sys.time(),"%Y-%m-%d_%H-%M-%S"),"_EWA-fit.RData",sep=""))






# for running simulations: run 10 or so at each unique parameter values
# for supercomputer: multithreading to speed up computation?
# command line arguments using commandArgs, and then fork() within a shell script to run different parameters?


# Mike's advice:
# You might put in the code to compute a correlation/RMSD between the average data and the model results. 
# That way you just the code return the particular combination of parameter and mode fit metrics.

# Then you can just to rerun the particular model combinations on your own machine.
