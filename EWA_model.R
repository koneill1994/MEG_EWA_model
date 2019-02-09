# EWA Model class

library(dplyr)
library(truncnorm)

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



# p_m, psd : parameter means, parameter sd's
# [delta, rho, lambda, phi]
p_m=c(1,1,1,1)
psd=c(.01,.01,.01,.01)

# initial choice probabilities based on human data
choice_prob_data=c(0.025, 0.100, 0.200, 0.250, 0.175, 0.075, 0.175)


model_data=data.frame()

if(TRUE){
  # gotta debug other parts first
  for(sim in 1:100){
    print(paste("sim ",sim))
    
    model_list=c(Make_EWA_model("1",p_m,psd,choice_prob_data),
                 Make_EWA_model("2",p_m,psd,choice_prob_data),
                 Make_EWA_model("3",p_m,psd,choice_prob_data),
                 Make_EWA_model("4",p_m,psd,choice_prob_data))
    
    for(round in 1:20){
      #print(paste("    round ",round))
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
        model_data=rbind(model_data, m_log)
      }
    }
  }

}