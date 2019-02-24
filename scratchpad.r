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
