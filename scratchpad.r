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


