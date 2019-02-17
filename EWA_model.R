# EWA Model class

library(dplyr)
library(ggplot2)
library(foreach)
library(doParallel)

cl=makeCluster(detectCores()-1)


clusterEvalQ(cl,{
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
        delta             <<- min(max(rnorm(1, mean = p_mean[1], sd = p_sd[1]),0),1)
        rho               <<- min(max(rnorm(1, mean = p_mean[2], sd = p_sd[2]),0),1)
        lambda            <<- min(max(rnorm(1, mean = p_mean[3], sd = p_sd[3]),0),1)
        phi               <<- min(max(rnorm(1, mean = p_mean[4], sd = p_sd[4]),0),1)
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
        }
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
  NULL
})

model_run=function(parameter_means, parameter_sds, choice_data, n_sims, h_dat){
  # run the model n_sims times to get a modelled dataset based on 
  # inputted parameters
  model_data_full=data.frame()
  start_time=Sys.time()
  
  for(sim in 1:n_sims){
    # so we don't spam the console
    if(sim%%10==0 & verbose){
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
                                    #se=sd(own_choice)/sqrt(length(own_choice)),
                                    agent_type="model"
                                    ),
                             agent_type,round,mean))
               ),
    data.frame(unique(select(mutate(group_by(h_dat, round),
                                    mean=mean(choice),
                                    #se=sd(choice)/sqrt(length(choice)),
                                    agent_type="human"
                                    ),
                             agent_type,round,mean))
               )
  )
  # reminder that for a full parameter space search
  # SE is not necessary and a pruned version of this function should be used
  
  
  # if you really want to look at an individual parameter set
  # and its fit to human data
  # if(drawgraphs){
  #   # look at it visually
  #   ggplot(compare_dat)+
  #     geom_line(aes(x=round, y=mean, color=agent_type))+
  #     geom_errorbar(mapping=aes(x=round, ymin=mean-se, ymax=mean+se, color=agent_type), 
  #                   width=.2)+
  #     labs("average choice")+
  #     labs(title="average choice by round for human and model agents",
  #          caption="error bars represent standard error")
  # }
  
  # dataframe to hold info about parameters and model fit
  # delta, rho, lambda, phi, initial_choice_data, rmse
  # and computation time in seconds
  
  # (add start time and end time to this as well)
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

setwd("F:/Downloads/MEG_EWA_model-master (1)/MEG_EWA_model-master")

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
number_of_sims=10

# loop through different parameter sets here
param_means= (0:1)/10





registerDoParallel(cl)

getDoParWorkers()




fit_data= foreach(d=param_means, .combine=rbind, .inorder=F, .packages=c("dplyr")) %:%
          foreach(rh=param_means, .combine=rbind, .inorder=F, .packages=c("dplyr")) %:%
          foreach(l=param_means, .combine=rbind, .inorder=F, .packages=c("dplyr")) %:%
          foreach(ph=param_means, .combine=rbind, .inorder=F, .packages=c("dplyr")) %dopar% {
            pm=c(d,rh,l,ph)
            if(verbose) print(pm)
            model_run(pm, psd, choice_prob_data, number_of_sims, human_data)
          }


# this is linear, but we want parallel
# for(d in param_means){
#   for(rh in param_means){
#     for(l in param_means){
#       for(ph in param_means){
#         pm=c(d,rh,l,ph)
#         if(verbose) print(pm)
#         fit_data=rbind(fit_data,
#                        model_run(pm, psd, choice_prob_data, number_of_sims, human_data)
#         )
#       }
#     }
#   }
# }


# View(fit_data)

saveRDS(fit_data, file=paste(format(Sys.time(),"%Y-%m-%d_%H-%M-%S"),"_EWA-fit.rds",sep=""))



stopCluster(cl)


# k=readRDS("2019-02-11_15-56-25_EWA-fit.rds")




# for supercomputer: multithreading to speed up computation?

# quick time estimate
# takes ~ 30s to run a batch of simulations
# and if we test out 10^4 parameters
# it'll take 30000 seconds
# or 8.3 processor-hours

# use parallel package? 
# https://www.r-bloggers.com/how-to-go-parallel-in-r-basics-tips/ 
# .combine = rbind to combine the rows into the final dataframe

# efficient nested foreach using %:%
# "The operator turns multiple foreach loops into a single loop, 
#   creating a single stream of tasks that can all be executed in parallel."

# library(doParallel)
# library(foreach)
# so use nested foreach with %:%
# and a %dopar% at the end
# with .combine=rbind
# within the nested loop:
#   run the sim
#   return the dataframe row
#   .combine=rbind will rbind them together
# and assign them to whatever you assign the result of the foreach loop to
# which will return the filled dataframe that we want

# to put on the supercomputer:
# tar.gz scp'ed onto it, unpacked to reveal:
  # EWA_model.R (this file)
  # RData representing the human data (do that instead of having the supercomputer run Data_Analysis.R)

# scp back the fit_data.rds file
# analyze that

# Mike's advice:
# You might put in the code to compute a correlation/RMSD between the average data and the model results. 
# That way you just the code return the particular combination of parameter and mode fit metrics.

# Then you can just to rerun the particular model combinations on your own machine.
