# EWA Model for the Minimum Effort Game

This is an R implementation based on Camerer's (1999) [Experience Weighted Attraction Learning Model](https://doi.org/10.1111/1468-0262.00054), or "EWA model" for short.  

We are using this algorithm to model performance in the minimum effort game, or MEG.  

The model itself has been modified from the original description to account for counterfactuals considered by the player.  

I'll be going through the different parts of the script here to aid myself and posterity in understanding my own code.  

## The Model Itself

The model is implemented as an [R5 reference class](http://adv-r.had.co.nz/R5.html) with the following fields:

| Field | Description |
|--|--|
| `choices` | The list of valid choices the model can make, characterized as strategies by Camerer |
| `attraction` | The attractions of the corresponding choices for the current round |
| `attraction_prev` | The attractions of the corresponding choices for the previous round |
| `N` | The N parameter (see Camerer 1999) for the current round |
| `N_prev` | The N parameter for the previous round |
| `delta` | The delta parameter |
| `rho` | The rho parameter |
| `lambda` | The lambda parameter |
| `phi` | Phi Parameter |
| `choice_prob` | Probability of choosing each possible choice.  Should add up to 1 |
| `own_choice` | The choice this agent made last |
| `id` | A unique (or not so unique) set to identify this model.  Doesn't have any effect in the math, only shows up in the logs |

and the methods:

| Method | Description |
|--|--|
| `initialize(id,p_mean, p_sd, initial_choice_prob)` | The constructor function which creates an EWA model instance, with an id of `id`, initial choice probabilites of `initial_choice_prob`, and delta, rho, lambda, and phi values generated from truncated normal distributions with means of `p_mean[1]`, `p_mean[2]`, `p_mean[3]`, and `p_mean[4]` (respectively), and standard deviations of `p_sd[1]`, `p_sd[2]`, `p_sd[3]`, `p_sd[4]` (respectively).  |
| `update(group_choices)` | The method which updates the internal state (i.e. the weighted attractions, and consequently the choice probabilities) of the model, based on the choices made by all the other players in the previous round (or alternatively, the strategies which all the other players employed in the previous round) |
| `choose()` | Makes a choice weighted by the choice probabilities held in `choice_prob` |
| `output_logfile()` | Returns a single-row dataframe containing the internal state of the model, mostly for debugging purposes.  `own_choice` will be grabbed from this output and used in subsequent analysis; the rest will be thrown away. |

## The run function

`model_run(parameter_means, parameter_sds, choice_data, n_sims, h_dat)` is the function which runs a group of `n_sims` simulations of models with parameters distributed with means of `parameter_means` and standard deviations of `parameter_sds`, and initial choice probability of `choice_data`, which in this model has been hardcoded to initial choice values based off of human data.  `h_dat` is a dataframe holding the human data which will be compared to the model-generated data to evaluate the goodness-of-fit of models with the given parameters.  

I'll describe some lines of note below:

---

`for(sim in 1:n_sims){` 

We loop over `n_sims` to run that many separate simulations, with unique models generated based on given parameters.  

---

```
model_list=c(Make_EWA_model("1",parameter_means,parameter_sds,choice_data),
                 Make_EWA_model("2",parameter_means,parameter_sds,choice_data),
                 Make_EWA_model("3",parameter_means,parameter_sds,choice_data),
                 Make_EWA_model("4",parameter_means,parameter_sds,choice_data))
```

We create a list, `model_list`, to hold all of our generated models.  In this code I have added 4 models, because the MEG we are modelling has 4 players.  You could add any number of models to this list, and the algorithm will run that many models per simulation. 

---

`for(round in 1:20)`

The MEG runs for 20 rounds, at least in our version, so the simulation runs for the same number of rounds.  

---

```
for(model in model_list){
  g_choices=c(g_choices,model$choose())
}
```

Iterate over all of the models, and have them make their initial choice.  

---

```
for(model in model_list){
  model$update(g_choices)
```

Update each of the models' internal states based on the choices made by themselves and the other models.  

---

```
header=data.frame(sim=sim, model=model$id, round=round)
m_log=cbind(header,model$output_logfile())
model_data_full=rbind(model_data_full, m_log)
```

Here we put together a dataframe to hold the choices made by each model for each round in each simulation, as well as their internal states (the latter just for debug purposes).  

---

```
model_data=model_data_full[,1:4]
```

Like I said, when comparing model data and human data, we don't really care about anything except the behavior of models.  We don't have internal states or parameter data for humans (I wish) so we just need the model behavior to compare to the human behavior.  

---

```
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
```

Assemble the human and model data into a single dataframe of aggregated data.  Uncomment the se code (and add se to the select function arguments) if you want error bars in the commented out ggplot code a few lines below this. 

---

```
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
```

Output the data from the model run in a single-row dataframe, which we'll rbind with all the other model run outputs later on.  Contains the average values for the parameters `delta`, `rho`, `lambda`, `phi`, as well as the `initial_choice_data` parameters, which in this case is the same for all models, but you could try different values for that if you wanted.  Secondly, it outputs `rmse`, the root mean squared difference between the human data and model data.  Lower values means better fit.  And lastly, it outputs `computation_time`, which is the amount of time it took to run this batch of models, which is mostly there for debug purposes.  

## Parallelization

