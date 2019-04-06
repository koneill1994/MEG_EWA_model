df=data.frame(delta=c(1,2,3), lambda=c(4,5,6), rho=c(7,8,9), phi=c(10,11,12), rmse=c(100,200,300))

checked_values=data.frame(delta=c(1,0), lambda=c(4,0), rho=c(7,0), phi=c(10,0), rmse=c(100,0))

df[
apply( # which rows of df are not present in checked_values
  apply(df[1:length(coords)],1,function(crd){ # which coordinates in df do not match something in checked_values
    !(
      apply(checked_values,1,function(x){ # which in checked_values match the given coordinate
        all(crd==x[1:length(crd)])
      })
    )
  }),2,all),]





#x axis is df
# y axis is checked values

# to find which of df is not present
# we want to grab the 

eval_function=hill_climber_eval

hc=MakeHillClimber(as.character(1), hill_climber_eval, step_size, F, bounds, human_data, model_params)

hc$hill_climb()


crd=coords


rmse(hc_dat[hc_dat$hc_id=="1",]$rmse)

plot(hc_dat[hc_dat$hc_id=="1" & hc_dat$chosen,]$round,
     hc_dat[hc_dat$hc_id=="1" & hc_dat$chosen,]$rmse,
     type="l")


library(ggplot2)

ggplot(hc_dat[hc_dat$chosen,],aes(x=round,y=rmse,group=hc_id, color=hc_id))+
  geom_line()

# need to improve stochastic method so that odds are greater for lower rmse
# too much randomness
# maybe add in a parameter there?


# hill_find=ifelse(seek_maxima, which.max, which.min)(df$rmse)

df=data.frame(rmse=c(1,2,3,4,5,6,7,8,9,10)/sum(c(1,2,3,4,5,6,7,8,9,10)))

seek_maxima=F

hill_find=ifelse(seek_maxima, which.max, which.min)(df$rmse)

hill_find=replace(rep(F,dim(df)[1]),sample(1:length(df$rmse), 1, prob=sum(df$rmse)/(df$rmse)),T)

sum(df$rmse)/(df$rmse)



replace(rep(F,dim(df)[1]),sample(1:length(df$rmse), 1, prob=sum(df$rmse)/(df$rmse)),T)




#####

# 4-6-19

parameter_means=c(.5,.5,.5,.5)
parameter_sds=c(.1,.1,.1,.1)
choice_data=c(0.025, 0.100, 0.200, 0.250, 0.175, 0.075, 0.175)
n_sims=5
h_dat=human_data

#     return(model_run(coords, 
# model_params$psd, 
# model_params$choice_prob_data, 
# model_params$number_of_sims, 
# human_data))


model_run(parameter_means,parameter_sds,choice_data,n_sims,human_data)


# Correlation of 
# mean of all choices for all 4 players for all 20 rounds within 1 simulation
# Variance of choice for all 4 players for all 20 rounds within 1 simulation
mean_var_corr=
  cor(
    aggregate(model_data$own_choice, by=list(sim=model_data$sim), FUN=mean)$x,
    aggregate(model_data$own_choice, by=list(sim=model_data$sim), FUN=var)$x
  )

mean_var_corr=
  cor(
    aggregate(h_dat$choice, by=list(sim=h_dat$sim), FUN=mean)$x,
    aggregate(h_dat$choice, by=list(sim=h_dat$sim), FUN=var)$x
  )




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
)

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

library(ggplot2)

# this will inspect the data
ggplot(hc_dat[hc_dat$chosen,], aes(x=round,y=mean_var_corr_abs_diff, group=hc_id, color=hc_id))+
         geom_line()
       
# lines(hc_dat[hc_dat$chosen,]$round,hc_dat[hc_dat$chosen,]$mean_var_corr_abs_diff)
