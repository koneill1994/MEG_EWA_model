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

[I'm gonna talk about the algorithm in depth here in a later edit]

## The run function

`model_run(parameter_means, parameter_sds, choice_data, n_sims, h_dat)` is the function which runs a group of `n_sims` simulations of models with parameters distributed with means of `parameter_means` and standard deviations of `parameter_sds`, and initial choice probability of `choice_data`, which in this model has been hardcoded to initial choice values based off of human data.  `h_dat` is a dataframe holding the human data which will be compared to the model-generated data to evaluate the goodness-of-fit of models with the given parameters.  

I'll describe some lines of note below:

---

```r
for(sim in 1:n_sims){
``` 

We loop over `n_sims` to run that many separate simulations, with unique models generated based on given parameters.  

---

```r
model_list=c(Make_EWA_model("1",parameter_means,parameter_sds,choice_data),
                 Make_EWA_model("2",parameter_means,parameter_sds,choice_data),
                 Make_EWA_model("3",parameter_means,parameter_sds,choice_data),
                 Make_EWA_model("4",parameter_means,parameter_sds,choice_data))
```

We create a list, `model_list`, to hold all of our generated models.  In this code I have added 4 models, because the MEG we are modelling has 4 players.  You could add any number of models to this list, and the algorithm will run that many models per simulation. 

---

```r
for(round in 1:20)
```

The MEG runs for 20 rounds, at least in our version, so the simulation runs for the same number of rounds.  

---

```r
for(model in model_list){
  g_choices=c(g_choices,model$choose())
}
```

Iterate over all of the models, and have them make their initial choice.  

---

```r
for(model in model_list){
  model$update(g_choices)
```

Update each of the models' internal states based on the choices made by themselves and the other models.  

---

```r
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

```r
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

```r
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

This is the fun part.  

As any good computer scientist will know, nested for-loops scale exponentially.  So lets have a look at the simple way to do this, and then we'll look at the fast way to do it.  

```r
for(d in param_means){
  for(rh in param_means){
    for(l in param_means){
      for(ph in param_means){
        pm=c(d,rh,l,ph)
        if(verbose) print(pm)
        fit_data=rbind(fit_data,
                       model_run(pm, psd, choice_prob_data, number_of_sims, human_data)
        )
      }
    }
  }
}
```

So we have 4 free parameters, `delta`, `rho`, `lambda`, and `phi`, which we want to explore.  So the number of individual combinations of each of these will be `n_delta*n_rho*n_lambda*n_phi`.  If we want to look at the same number of means for each parameter, we will need to loop through `n_means^4`, which gets unmanageable very quickly.  Observe:

| Number of means | Number of individual cases to check |
|--|--|
| 5 | 625 |
| 10 | 10,000 |
| 20 | 16,000 |
| 50 | 6,250,000 |
| 100 | 100,000,000 |
| 500 | 62,500,000,000 |
| 1000 | 1,000,000,000,000 |

You get the idea.  Any tiny optimization we can make, even if it only saves microseconds per model run, will add up over the length of the model run.  One of the biggest time-saving methods is to parallelize; to split up the process into smaller sub-processes which can run independently.  And this algorithm is a prime candidate for this because each model run only needs to know the parameters its assigned; it doesn't need anything determined by previous model runs.  

Let me further prove my point.  One run of 100 simulations takes about 30 seconds on my machine.  So if I want to look through 10 different values for each parameter, the length of time it will take is `30 seconds * 10^4`, which is 30,000 seconds, or 8.33 hours.  Lets look at an estimate of the time the algorithm will take for a given number of separate processes exploring the parameter space (assuming each process is running on a dedicated core).  

| number of processes | estimated run-time |
|--|--|
| 1 | 8.33 hours |
| 2 | 4.167 hours |
| 3 | 2.78 hours |
| 4 | 2.08 hours |
| 8 | 1.04 hours |
| 12 | 41.67 minutes |
| 16 | 31.25 minutes |
| 32 | 15.625 minutes |
| 100 | 5 minutes |

If we can parallelize the algorithm, and get it onto a machine which has a large number of processor cores (such as a supercomputer), we can get a significant reduction in the amount of time needed to search the space.  

If you want a better understanding of this idea and applying it to R, [this tutorial rocks.](https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html)

I'm going to explain here what my code does to allow for parallelization.  

---

```r
library(foreach)
library(doFuture)
```

Important!  Ya gotta have the packages if you wanna do any of this.  

`foreach` gives us access to [foreach loops](https://cran.r-project.org/web/packages/foreach/vignettes/foreach.pdf), which work like [foreach loops in javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach) or like [for loops in python](https://wiki.python.org/moin/ForLoop).  These have two very important properties which will allow us to parallelize really well, and I'll talk about that further down.  

`doFuture` is an awesome library which works like `doParallel` but automatically includes all needed environment variables in the child processes.  `doParallel` will allow you you split off child processes in R, but you have to explicitly define the objects which will be sent to the child process.  `doFuture` does it without us having to figure out what all the child process needs, which is nice because there's a fair number of objects that need to be copied to the child processes.  

---

`registerDoFuture()`

You gotta do this to initialize the doFuture module to let you run this on multiple cores.  Don't ask me why, you just have to.  Them's the rules.  

---

```r
fit_data= foreach(d=param_means, .combine=rbind) %:%
          foreach(rh=param_means, .combine=rbind) %:%
          foreach(l=param_means, .combine=rbind) %:%
          foreach(ph=param_means, .combine=rbind) %dopar% {
            pm=c(d,rh,l,ph)
            if(verbose) print(pm)
            model_run(pm, psd, choice_prob_data, number_of_sims, human_data)
          }
```

This is it.  This is the heart of the parallelization code.  Its equal parts beautiful and terrifying, I know.  Calm down and I'll break it down for you.  

In general, the structure is similar to the linear nested for loops you saw above, just with foreach loops instead of for loops.  But remember those two properties foreach loops have that I said were really useful?  Here they are:

`%dopar%` is the operator which spins the stuff within the loop into its own process.  If you wanted to run the above code in a serial manner, you could replace it with `%do%`, which would run just fine.  

`%:%` is the operator which lets you "un-nest" nested foreach loops, so that you are looping over each combination of all of the looping variables in one loop, instead of 4 nested loops.  In serial processing this doesn't really make a difference, but it becomes very important in parallel.  

Consider this:

```r
fit_data= foreach(d=param_means, .combine=rbind) %dopar% {
          foreach(rh=param_means, .combine=rbind) %dopar% {
          foreach(l=param_means, .combine=rbind) %dopar% {
          foreach(ph=param_means, .combine=rbind) %dopar% {
            pm=c(d,rh,l,ph)
            if(verbose) print(pm)
            model_run(pm, psd, choice_prob_data, number_of_sims, human_data)
          }}}}
```

The first `%dopar%` loop will spin off a number of processes to run the code which follows.  The number of child processes isn't important; you can change it with parameters in `registerDoFuture()`, and the default is different for each OS.  For Windows the default is 3, so we'll assume that.  

So in Windows, the first `%dopar%` will create 3 child processes, each of which will run all the code within its loop, and output the value from within.  

The second `%dopar%` will spin off 3 child processes each time its run.  This means we'll have the original process, the first 3 child processes, as well as `3*3` child processes, 3 created by each first level child process.  That means we'll have 13 processes, and only 9 of them are running any code.  The others are just managing their child processes and waiting for them to return values.  

The third `%dopar%` will spin off 3 children each, which is a total of `1 + 3 + 3*3 + 3*3*3`, which is 40, and only 27 are running code.  
The fourth and final `%dopar%` will spin off 3 children each, leading to a total of `sum(3^(0:4))`, which is 121 processes total, only 81 are running code.  

We end up with a bunch of processes which are doing nothing but wait for their children processes to finish, which isn't really useful or necessary, and will slow things down and eat up memory without offering anything useful.  Using `%:%`, we have the main process as the parent, and we have all of our allocated child processes running the stuff we want them to.  

**Last thing to mention:** foreach loops have a return value.  Setting the `.combine` argument to `rbind` will perform the rbind operation to each value returned from the code inside, which in this case is the output of `model_run()`.  And if you recall above, the output of `model_run()` is a single-row dataframe containing all we want to know about the parameters of that model run and its fit to the human data.  So when this code finishes it will sew all of our model outputs in a nice neat dataframe, showing us the shape of the goodness-of-fit space, and letting us figure out which parameters fit the human data best.  
