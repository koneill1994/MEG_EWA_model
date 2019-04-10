# EWA Model class

program_start_time=Sys.time()

library(dplyr)
library(foreach)
library(doParallel)

packs=c("dplyr","foreach","doParallel")

# fiddle with this in case it doesn't work on your machine
cl=makeCluster(detectCores(logical = FALSE))

# run an eval on the following code
# so that its present in the enviroments of all the workers
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
          
  
          if(sum(is.infinite(exp(lambda * attraction)))){
            # convenience code to catch examples that would be coerced to Inf
            #   i don't like it either but we have to run this on finite computers
            #   not in infinite abstract mathematical space
            
            # assumption: a value of Inf will be infinitely greater than the other values
            # not tenable?
            choice_prob<<-ifelse(exp(lambda * attraction)==Inf,1,0)
          } else if(sum(exp(lambda * attraction)) == 0){
            # if somehow all the attractions are so far negative that they
            #   all exponentiate as -Inf, just make choices randomly
            choice_prob   <<- rep(1/length(attraction),length(attraction))
          } else if(sum(exp(lambda * attraction)) == Inf){
            # numbers can sum to Inf but each individually be less than Inf
            # this should function identically to the next block except it handles Inf sum better
            la=exp(lambda * attraction)/max(exp(lambda * attraction))
            choice_prob   <<- la/sum(la)
          } else {
            # normal EWA operation
            choice_prob   <<- exp(lambda * attraction) / sum( exp(lambda * attraction) )
          }
          attraction_prev <<- attraction
          N_prev          <<- N
        }
      },
      choose= function(){
        # too few positive probabilities
        # if they're all 0 just choose randomly
        # if this triggers its a sign something went wrong
        if(sum(choice_prob)==0){
          own_choice      <<- sample(choices, 1,replace=T)
        }
        
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
  
  MakeHillClimber=setRefClass(
    "Hill_Climber",
    fields=list(
      coords="numeric",
      eval_function="function",
      step_size="numeric",
      seek_maxima="logical",
      bounds="numeric",
      hc_id="character",
      human_data="data.frame",
      checked_values="data.frame",
      iter="numeric",
      model_params="list"
      ),
    methods=list(
      # bounds
      # p1min p1max
      # p2min p2max
      # p3min p3max
      initialize=function(hc_id, evalf, step_s, seek_max,bounds, hd,mp){
        hc_id         <<- hc_id
        eval_function <<- evalf
        step_size     <<- step_s
        seek_maxima   <<- seek_max
        coords        <<- apply(bounds,1,function(x){return(runif(1,x[1],x[2]))})
        human_data    <<- hd
        iter          <<- 0
        checked_values<<- data.frame()
        model_params  <<- mp
        },
      hill_climb=function(){
        # get a list of all adjacent coordinates
        adjacent=foreach(d=1:length(coords), .combine=rbind) %:%
          foreach(ss=c(-step_size, step_size), .combine=rbind) %do% {
            replace(coords,d,coords[d]+ss)
          }
        
        # lookup table of previously checked values
        # to save on computation time
        df=data.frame()
        for(ci in 1:nrow(adjacent)){
          crd=adjacent[ci,]
          llist=apply(checked_values,1,function(x){all(crd==x[1:length(coords)])})
          if(any(llist)){
            df=rbind(df,subset(checked_values,llist,1:ncol(checked_values)))
          } else {
            newrow=eval_function(crd,human_data, model_params)
            checked_values <<- rbind(checked_values,setNames(newrow,names(checked_values)))
            df=rbind(df,newrow)
          }
        }
        
        
        

        # find the min or max of the rmse's
        # hill_find=ifelse(seek_maxima, which.max, which.min)(df$mean_var_corr_abs_diff)
        
        # per mike's new change, we are now optimizing on mean_var_corr_abs_diff
        
        hill_find=replace(rep(F,dim(df)[1]),
                          sample(1:length(df$mean_var_corr_abs_diff), 
                                 1, 
                                 prob=df$mean_var_corr_abs_diff^(1/(model_params$stoch*ifelse(seek_maxima, 1, -1)))/
                                   sum(df$mean_var_corr_abs_diff^(1/(model_params$stoch*ifelse(seek_maxima, 1, -1))))
                          ),
                          T)
        # bounds of rmse
        
        
        df$chosen=hill_find
        df$hc_id=hc_id
        df$round=iter
        
        coords <<- adjacent[hill_find,]
        iter   <<- iter+1
        # return the evaluated points so we can look at that later
        return(df)
      }
      )
    )
  
  model_run=function(parameter_means, parameter_sds, choice_data, n_sims, h_dat){
    # run the model n_sims times to get a modelled dataset based on 
    # inputted parameters
    model_data_full=data.frame()
    start_time=Sys.time()
    
    for(sim in 1:n_sims){
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
                                      agent_type="model",
                                      mean_var_corr=
                                        cor(
                                          aggregate(model_data$own_choice, by=list(sim=model_data$sim), FUN=mean)$x,
                                          aggregate(model_data$own_choice, by=list(sim=model_data$sim), FUN=var)$x
                                        )
                                      ),
                               agent_type,round,mean,mean_var_corr))
                 ),
      data.frame(unique(select(mutate(group_by(h_dat, round),
                                      mean=mean(choice),
                                      agent_type="human",
                                      mean_var_corr=
                                        cor(
                                          aggregate(h_dat$choice, by=list(sim=h_dat$group), FUN=mean)$x,
                                          aggregate(h_dat$choice, by=list(sim=h_dat$group), FUN=var)$x
                                        )
                                      ),
                               agent_type,round,mean,mean_var_corr))
                 )
    )
    end_time=Sys.time()
    
    return(
      data.frame( delta=mean(model_data_full$delta),
                  delta_sd=sd(model_data_full$delta),
                  rho=mean(model_data_full$rho),
                  rho_sd=sd(model_data_full$rho),
                  lambda=mean(model_data_full$lambda),
                  lambda_sd=sd(model_data_full$lambda),
                  phi=mean(model_data_full$phi),
                  phi_sd=sd(model_data_full$phi),
                  initial_choice_data=I(list(choice_data)),
                  rmse=sqrt(mean(( compare_dat[compare_dat$agent_type=="human",]$mean-
                                     compare_dat[compare_dat$agent_type=="model",]$mean)^2)),
                  human_mean_var_corr=mean(compare_dat[compare_dat$agent_type=="human",]$mean_var_corr),
                  model_mean_var_corr=mean(compare_dat[compare_dat$agent_type=="model",]$mean_var_corr),
                  mean_var_corr_abs_diff=mean(abs(compare_dat[compare_dat$agent_type=="human",]$mean_var_corr-
                                               compare_dat[compare_dat$agent_type=="model",]$mean_var_corr)),
                  start=start_time,
                  end=end_time,
                  computation_time=end_time-start_time
      )
    )
  }
  
  
  hill_climber_eval=function(coords, human_data, model_params){

    return(model_run(coords, model_params$psd, model_params$choice_prob_data, model_params$number_of_sims, human_data))
  }
  
})










load_human_dat=T

mode_n=2
mode=c("full","hc")[mode_n]

# setwd("/home/kevin/Documents/ewa/MEG_EWA_model")
setwd("C:/Users/Kevin/Dropbox/minimum_effort_game/EWA_Model")
# setwd("E:/Libraries/r projects/MEG_EWA_model-master")

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
  # human_data=readRDS("MEG_human_data.rds")
  # setwd("C:/Users/Kevin/Dropbox/minimum_effort_game")
  
  human_data=read.csv("MEG_Group_Data_Kevin_Format.csv")[,2:5]
  colnames(human_data)=c("group","subject","round","choice")
}


model_params=list(
  psd=c(.1,.1,.1,.1),
  
  # initial choice probabilities based on human data
  choice_prob_data=c(0.05882353, 0.07352941, 0.17647059, 0.22058824, 0.22058824, 0.10294118, 0.14705882),
  
  # number of simulations to run
  # number_of_sims=75,
  number_of_sims=10,
  
  # stochasticity parameter
  # minimum is zero
  # higher is more random
  # 1 is direct proportion to evaluation values
  stoch=.25
)


# number of means to search
# i.e. resolution of the parameter space
num_means=10

# loop through different parameter sets here
param_means= seq(0,1,by=1/(num_means-1))


# set up the parallel computation
registerDoParallel(cl)
getDoParWorkers()

if(mode=="full"){
  
  # the heart of the whole thing: the exhaustive parameter space search
  # this part should take minutes to hours depending on num_means and number_of_sims
  fit_data= foreach(d=param_means, .combine=rbind, .inorder=F, .packages=c("dplyr")) %:%
    foreach(rh=param_means, .combine=rbind, .inorder=F, .packages=c("dplyr")) %:%
    foreach(l=param_means, .combine=rbind, .inorder=F, .packages=c("dplyr")) %:%
    foreach(ph=param_means, .combine=rbind, .inorder=F, .packages=c("dplyr")) %dopar% {
      pm=c(d,rh,l,ph)
      model_run(pm, model_params$psd, model_params$choice_prob_data, model_params$number_of_sims, human_data)
    }
  
  # View(fit_data)
  
  saveRDS(fit_data, file=paste(format(Sys.time(),"%Y-%m-%d_%H-%M-%S"),"_EWA-fit.rds",sep=""))
  
} else if (mode=="hc"){
  
  #num_climbers=1
  num_climbers=15
  climber_iterations=100

  step_size=.001
  
  bounds=matrix(rep(c(0,1),each=4), nrow=4)
  
  hc_dat = foreach(climber=1:num_climbers, .combine=rbind, .inorder=F, .packages=packs) %dopar% {
    
    # send info to run_model properly
    
    hc=MakeHillClimber(as.character(climber), hill_climber_eval, step_size, F, bounds, human_data, model_params)

    # initialize a smaller df to hold one climber's data
    # replace hc_dat here with the smaller df
    # return the smaller df so it gets rbinded for the output
    climber_dat=data.frame()
    
    for(it in 1:climber_iterations){
      climber_dat=rbind(climber_dat,hc$hill_climb())
    }
    return(climber_dat)
  }
  
  
  program_end_time=Sys.time()
  
  #save the fit data (IMPORTANT)
  saveRDS(hc_dat, file=paste("./data/",format(program_end_time,"%Y-%m-%d_%H-%M-%S"),"_EWA-hc.rds",sep=""))
  

  # write a log of the variables to keep track of config 
  fileConn=file(paste("./data/",format(program_end_time,"%Y-%m-%d_%H-%M-%S"),"_hc-configLog.tsv",sep=""))
  writeLines(c(paste("program_start_time",program_start_time,sep="\t"),
               paste("program_end_time",program_end_time,sep="\t"),
               paste("num_workers",getDoParWorkers(),sep="\t"),
               paste("mode",mode,sep="\t"),
               paste("num_means",num_means,sep="\t"),
               paste("means",toString(param_means),sep="\t"),
               paste("sd's",toString(model_params$psd),sep="\t"),
               paste("choice_prob_data",toString(model_params$choice_prob_data),sep="\t"),
               paste("number_of_sims",model_params$number_of_sims,sep="\t"),
               paste("hc_stochasticity",model_params$stoch,sep="\t"),
               paste("num_climbers",num_climbers,sep="\t"),
               paste("climber_iterations",climber_iterations,sep="\t"),
               paste("step_size",step_size,sep="\t"),
               paste("bounds",toString(bounds),sep="\t"),
               paste("r_version",R.version.string,sep="\t"),
               paste("pc_info",toString(Sys.info()),sep="\t")
  ), 
  fileConn)
  close(fileConn)
}
  
  

stopCluster(cl)