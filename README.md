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

Lets have a look through the update function just to know how it works.  

```r
update=function(group_choices){
  cl_0=unique(group_choices)
  choice_list=sample(cl_0,length(cl_0))

  for(g_min in choice_list){
    delta_n = ifelse(choices == own_choice, 1, delta )
    max_payoff =  ((g_min - 1) * 10) + 70

    payoff = ifelse( choices < g_min, ((choices - 1) * 10) + 70, max_payoff - (choices-g_min)*10 )

    weighted_payoff = delta_n*payoff

    N               <<- rho*N_prev+1
    attraction      <<- (phi*N_prev*attraction_prev + weighted_payoff)/N


    if(sum(exp(lambda * attraction)) == Inf){
      choice_prob<<-ifelse(exp(lambda * attraction)==Inf,1,0)
    } else if(sum(exp(lambda * attraction)) == 0){
      choice_prob   <<- rep(1/length(attraction),length(attraction))
    } else{
      choice_prob   <<- exp(lambda * attraction) / sum( exp(lambda * attraction) )
    }
    attraction_prev <<- attraction
    N_prev          <<- N
  }
},
```

Don't freak out, its not too hard.  Lets break it down.  

---

```r
update=function(group_choices){
  cl_0=unique(group_choices)
  choice_list=sample(cl_0,length(cl_0))

  for(g_min in choice_list){
```

As input, this function takes `group_choices`, the set of all choices made (strategies chosen, etc) by all players this round.  We want our model to look at the counterfactuals (this is the alteration to Camerer's original algorithm), so we want it to consider what the model's payout would be in different scenarios, not just in the scenario that actually happened.  Humans do this kind of hypothetical reasoning all the time, so its good to capture that in our model.  

The hypothetical scenarios, i.e. the counterfactuals, that the model will be considering is scenarios in which the model's choice remained the same, but the minimum effort chosen would be different.  Specifically, the model should look at the payoff it would get if the minimum effort in the group is one of the choices made by one of the players.  One could argue whether or not these counterfactuals are weighted in a human's mind similarly to how actual occurences are weighted, but for simplicity's sake they will affect the model in the same way.  

So, in summary, we want to go through each of the choices made, and update our choice probabilities when considering how we would have done if that choice was the minimum.  So here we go.  

---

```r
cl_0=unique(group_choices)
```

Only grab the unique choices; no need to look at a choice of `5` three times if three people played a `5`.  It will give us the same outcome.  

---

```r
choice_list=sample(cl_0,length(cl_0))
```

Randomize the order in which we consider each possible scenario.  One could argue that humans don't necessarily randomly look at or consider each counterfactual, but just to keep things constant and eliminate order effects, we shuffle this list.  

---

```r
for(g_min in choice_list){
```

Finally, we loop through each possible counterfactual (which includes the scenario which actually occurred).  

---

 ```r
delta_n = ifelse(choices == own_choice, 1, delta )
max_payoff =  ((g_min - 1) * 10) + 70

payoff = ifelse( choices < g_min, ((choices - 1) * 10) + 70, max_payoff - (choices-g_min)*10 )

weighted_payoff = delta_n*payoff

N               <<- rho*N_prev+1
attraction      <<- (phi*N_prev*attraction_prev + weighted_payoff)/N
```

Next we do the bulk of our internal state manipulation.  

---

```r
delta_n = ifelse(choices == own_choice, 1, delta )
```

If the choice we're considering is our own choice, set `delta_n` to 1, otherwise set it to our `delta` parameter value.  Consult Camerer on why we do this, but we're setting this so we know what to multiply our payoffs against to get our weighted payoffs.  

---

```r
max_payoff =  ((g_min - 1) * 10) + 70
payoff = ifelse( choices < g_min, ((choices - 1) * 10) + 70, max_payoff - (choices-g_min)*10 )
```

This is our payoff function.  In future versions of the model, the payoff function will be passed to the model as an argument; as it is now, we've hardcoded the MEG payoff function.  

---

```r
weighted_payoff = delta_n*payoff
```

This gives us our weighted payoffs, which we will turn into attractions via:

```r
N               <<- rho*N_prev+1
attraction      <<- (phi*N_prev*attraction_prev + weighted_payoff)/N
```

---

```r
if(sum(exp(lambda * attraction)) == Inf){
  choice_prob<<-ifelse(exp(lambda * attraction)==Inf,1,0) # 2
} else if(sum(exp(lambda * attraction)) == 0){
  choice_prob   <<- rep(1/length(attraction),length(attraction)) # 3
} else{
  choice_prob   <<- exp(lambda * attraction) / sum( exp(lambda * attraction) ) # 1
}
```

This is the part that gets us our choice probabilities, based on our attractions and lambda.  It makes most sense for me to explain this out of the order given in the code, so I'm gonna do it the order commented after the lines above.  

---

```r
choice_prob   <<- exp(lambda * attraction) / sum( exp(lambda * attraction) ) # 1
```

Easy stuff, right?  This gives us the probability of choosing any given option.  Because we have an item in a list divided by the sum of items in that list, they will all add up to 1.  That way we have a nice and simple way to get weighted probabilities over a choice list.  This code will get you where you want to go in 99% percent of cases.  In the remaining 1% though, we have to get a bit creative.  

---

```r
if(sum(exp(lambda * attraction)) == Inf){
  choice_prob<<-ifelse(exp(lambda * attraction)==Inf,1,0) # 2
}
```

Now if you're like me, the mathematical part of your brain shudders and wonders what sort of madman would compare a finite number against infinity.  Well I am that madman, and the exponential function made me this way.  

Since we are running this on a finite computer, we don't have unlimited space to play around with.  There's only so big of a number you can toss around before your computer runs out of memory to hold that number.  On my machine at least, you can only count up to about `1.8*10^308` before R says "Screw it, just call it infinite, its too big for me to do math on it."  

Since the exponential function gets real big real fast, there's a point where a seemingly modest attraction value like `6000` will exponentiate somewhere above R's cutoff value, and R will classify it as `Inf` and refuse to give any more detail.  So to deal with these situations where the values get "effectively" infinite, we have to use some logic to get around it.  For example:

```r
x=c(10,10,10,6000,10,10)
exp(x)/sum(exp(x))
```

`x[4]` gives us `Inf/Inf` which gives us `NaN`, which isn't really a useful result.  But if we think about it, `x[4]`'s attraction is so astronomically large in comparison to the other attractions, its extremely unlikely anything but `x[4]` will be picked.  Remember, we're dealing with exponents here; its not `6000/10 = 600` times more likely to get picked, its `exp(6000)/exp(10)` times more likely to get picked, which is `exp(5990)` times more likely.  That number is monstrously, unfathomably large, its basically not even worth considering the other alternatives.  

So in these situations, we break the mathematical elegance for ease of computation.  

```r
if(sum(exp(lambda * attraction)) == Inf){
```

Check to see if the sum of `exp(lambda*attraction)` is equal to infinity, which would indicate that there is an infinite value somewhere in there.  

```r
choice_prob<<-ifelse(exp(lambda * attraction)==Inf,1,0) # 2
```

The choices which have effectively infinite exponentiated attraction are set to 1, and all others are set to 0.  You may complain that probabilities can't be exactly 1 or 0, but don't worry, the quantities that separate .99999... and 0.0000...001 from 1 and 0 are just too small for the computer to keep track of, just like `exp(6000)` is too big for the computer to keep track of.  In theory there's a difference, but in practice there might as well not be.  

---

```r
else if(sum(exp(lambda * attraction)) == 0){
  choice_prob   <<- rep(1/length(attraction),length(attraction)) # 3
}
```

With the above explanation out of the way, this one is pretty simple.  The MEG doesn't have negative payoffs, but it is possible to have negative payoffs in theory within the EWA model.  The normal math will still work out if there is at least one non-negative payoff.  For `y=exp(x)`, as x nears `-Inf`, y nears `0`.  So the sum on the bottom will still be something other than zero, giving a normal answer.  Observe:

```r
x=c(10,10,10,-6000,10,10)
exp(x)/sum(exp(x))
# [1] 0.2 0.2 0.2 0.0 0.2 0.2
```

We only run into trouble when all of the attraction values near `-Inf`.  

```r
x=c(-6000,-6000,-6000,-6000,-6000,-6000)
exp(x)/sum(exp(x))
```

We end up with a bunch of `0/0`'s, which evaluate to `NaN`, since they divide by `0`.  But if you think about it, if you are so massively disincentivized to choose any possible option, to the extent that modern computers can't even evaluate what you should do, then you should probably quit playing that game.  But since you can't (since quitting the game would in and of itself count as a strategy), you might as well just choose randomly.  Believe me, whatever you choose doesn't matter.  They're all gonna suck.  

```r
choice_prob   <<- rep(1/length(attraction),length(attraction)) # 3
```

---

Lastly, we have this:

```r
attraction_prev <<- attraction
N_prev          <<- N
```

This one is fairly trivial.  These variables represent the values from last round, so we might as well set them at the end of this round.  We're gonna want them later.  

I should also take this opportunity to point out that we're using `<<-` instead of `<-` or `=` here.  `<<-` is the "non-local variable assignment operator" in R, which means we have to use it when we're setting variables outside of our current [scope](https://en.wikipedia.org/wiki/Scope_(computer_science)).  Normally this doesn't matter in R, because almost all values in R are passed [by value, not by reference](https://stackoverflow.com/questions/373419/whats-the-difference-between-passing-by-reference-vs-passing-by-value).  Reference classes, as would be expected by their name, work differently.  When we run things in a member method of a reference class, we want to be setting variables in the environment of the instance itself, not in the environment of the function.  If we don't use this special operator, we would create local variables in the function's environment, which will be de-referenced and garbage collected when that function finished computation.  

Long story short, when setting fields in a reference class, don't forget to use `<<-` and not `<-` or `=`; otherwise you won't change the values you're trying to.  

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
=======
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

Lets have a look through the update function just to know how it works.  

```r
update=function(group_choices){
  cl_0=unique(group_choices)
  choice_list=sample(cl_0,length(cl_0))

  for(g_min in choice_list){
    delta_n = ifelse(choices == own_choice, 1, delta )
    max_payoff =  ((g_min - 1) * 10) + 70

    payoff = ifelse( choices < g_min, ((choices - 1) * 10) + 70, max_payoff - (choices-g_min)*10 )

    weighted_payoff = delta_n*payoff

    N               <<- rho*N_prev+1
    attraction      <<- (phi*N_prev*attraction_prev + weighted_payoff)/N


    if(sum(exp(lambda * attraction)) == Inf){
      choice_prob<<-ifelse(exp(lambda * attraction)==Inf,1,0)
    } else if(sum(exp(lambda * attraction)) == 0){
      choice_prob   <<- rep(1/length(attraction),length(attraction))
    } else{
      choice_prob   <<- exp(lambda * attraction) / sum( exp(lambda * attraction) )
    }
    attraction_prev <<- attraction
    N_prev          <<- N
  }
},
```

Don't freak out, its not too hard.  Lets break it down.  

---

```r
update=function(group_choices){
  cl_0=unique(group_choices)
  choice_list=sample(cl_0,length(cl_0))

  for(g_min in choice_list){
```

As input, this function takes `group_choices`, the set of all choices made (strategies chosen, etc) by all players this round.  We want our model to look at the counterfactuals (this is the alteration to Camerer's original algorithm), so we want it to consider what the model's payout would be in different scenarios, not just in the scenario that actually happened.  Humans do this kind of hypothetical reasoning all the time, so its good to capture that in our model.  

The hypothetical scenarios, i.e. the counterfactuals, that the model will be considering is scenarios in which the model's choice remained the same, but the minimum effort chosen would be different.  Specifically, the model should look at the payoff it would get if the minimum effort in the group is one of the choices made by one of the players.  One could argue whether or not these counterfactuals are weighted in a human's mind similarly to how actual occurences are weighted, but for simplicity's sake they will affect the model in the same way.  

So, in summary, we want to go through each of the choices made, and update our choice probabilities when considering how we would have done if that choice was the minimum.  So here we go.  

---

```r
cl_0=unique(group_choices)
```

Only grab the unique choices; no need to look at a choice of `5` three times if three people played a `5`.  It will give us the same outcome.  

---

```r
choice_list=sample(cl_0,length(cl_0))
```

Randomize the order in which we consider each possible scenario.  One could argue that humans don't necessarily randomly look at or consider each counterfactual, but just to keep things constant and eliminate order effects, we shuffle this list.  

---

```r
for(g_min in choice_list){
```

Finally, we loop through each possible counterfactual (which includes the scenario which actually occurred).  

---

 ```r
delta_n = ifelse(choices == own_choice, 1, delta )
max_payoff =  ((g_min - 1) * 10) + 70

payoff = ifelse( choices < g_min, ((choices - 1) * 10) + 70, max_payoff - (choices-g_min)*10 )

weighted_payoff = delta_n*payoff

N               <<- rho*N_prev+1
attraction      <<- (phi*N_prev*attraction_prev + weighted_payoff)/N
```

Next we do the bulk of our internal state manipulation.  

---

```r
delta_n = ifelse(choices == own_choice, 1, delta )
```

If the choice we're considering is our own choice, set `delta_n` to 1, otherwise set it to our `delta` parameter value.  Consult Camerer on why we do this, but we're setting this so we know what to multiply our payoffs against to get our weighted payoffs.  

---

```r
max_payoff =  ((g_min - 1) * 10) + 70
payoff = ifelse( choices < g_min, ((choices - 1) * 10) + 70, max_payoff - (choices-g_min)*10 )
```

This is our payoff function.  In future versions of the model, the payoff function will be passed to the model as an argument; as it is now, we've hardcoded the MEG payoff function.  

---

```r
weighted_payoff = delta_n*payoff
```

This gives us our weighted payoffs, which we will turn into attractions via:

```r
N               <<- rho*N_prev+1
attraction      <<- (phi*N_prev*attraction_prev + weighted_payoff)/N
```

---

```r
if(sum(exp(lambda * attraction)) == Inf){
  choice_prob<<-ifelse(exp(lambda * attraction)==Inf,1,0) # 2
} else if(sum(exp(lambda * attraction)) == 0){
  choice_prob   <<- rep(1/length(attraction),length(attraction)) # 3
} else{
  choice_prob   <<- exp(lambda * attraction) / sum( exp(lambda * attraction) ) # 1
}
```

This is the part that gets us our choice probabilities, based on our attractions and lambda.  It makes most sense for me to explain this out of the order given in the code, so I'm gonna do it the order commented after the lines above.  

---

```r
choice_prob   <<- exp(lambda * attraction) / sum( exp(lambda * attraction) ) # 1
```

Easy stuff, right?  This gives us the probability of choosing any given option.  Because we have an item in a list divided by the sum of items in that list, they will all add up to 1.  That way we have a nice and simple way to get weighted probabilities over a choice list.  This code will get you where you want to go in 99% percent of cases.  In the remaining 1% though, we have to get a bit creative.  

---

```r
if(sum(exp(lambda * attraction)) == Inf){
  choice_prob<<-ifelse(exp(lambda * attraction)==Inf,1,0) # 2
}
```

Now if you're like me, the mathematical part of your brain shudders and wonders what sort of madman would compare a finite number against infinity.  Well I am that madman, and the exponential function made me this way.  

Since we are running this on a finite computer, we don't have unlimited space to play around with.  There's only so big of a number you can toss around before your computer runs out of memory to hold that number.  On my machine at least, you can only count up to about `1.8*10^308` before R says "Screw it, just call it infinite, its too big for me to do math on it."  

Since the exponential function gets real big real fast, there's a point where a seemingly modest attraction value like `6000` will exponentiate somewhere above R's cutoff value, and R will classify it as `Inf` and refuse to give any more detail.  So to deal with these situations where the values get "effectively" infinite, we have to use some logic to get around it.  For example:

```r
x=c(10,10,10,6000,10,10)
exp(x)/sum(exp(x))
```

`x[4]` gives us `Inf/Inf` which gives us `NaN`, which isn't really a useful result.  But if we think about it, `x[4]`'s attraction is so astronomically large in comparison to the other attractions, its extremely unlikely anything but `x[4]` will be picked.  Remember, we're dealing with exponents here; its not `6000/10 = 600` times more likely to get picked, its `exp(6000)/exp(10)` times more likely to get picked, which is `exp(5990)` times more likely.  That number is monstrously, unfathomably large, its basically not even worth considering the other alternatives.  

So in these situations, we break the mathematical elegance for ease of computation.  

```r
if(sum(exp(lambda * attraction)) == Inf){
```

Check to see if the sum of `exp(lambda*attraction)` is equal to infinity, which would indicate that there is an infinite value somewhere in there.  

```r
choice_prob<<-ifelse(exp(lambda * attraction)==Inf,1,0) # 2
```

The choices which have effectively infinite exponentiated attraction are set to 1, and all others are set to 0.  You may complain that probabilities can't be exactly 1 or 0, but don't worry, the quantities that separate .99999... and 0.0000...001 from 1 and 0 are just too small for the computer to keep track of, just like `exp(6000)` is too big for the computer to keep track of.  In theory there's a difference, but in practice there might as well not be.  

---

```r
else if(sum(exp(lambda * attraction)) == 0){
  choice_prob   <<- rep(1/length(attraction),length(attraction)) # 3
}
```

With the above explanation out of the way, this one is pretty simple.  The MEG doesn't have negative payoffs, but it is possible to have negative payoffs in theory within the EWA model.  The normal math will still work out if there is at least one non-negative payoff.  For `y=exp(x)`, as x nears `-Inf`, y nears `0`.  So the sum on the bottom will still be something other than zero, giving a normal answer.  Observe:

```r
x=c(10,10,10,-6000,10,10)
exp(x)/sum(exp(x))
# [1] 0.2 0.2 0.2 0.0 0.2 0.2
```

We only run into trouble when all of the attraction values near `-Inf`.  

```r
x=c(-6000,-6000,-6000,-6000,-6000,-6000)
exp(x)/sum(exp(x))
```

We end up with a bunch of `0/0`'s, which evaluate to `NaN`, since they divide by `0`.  But if you think about it, if you are so massively disincentivized to choose any possible option, to the extent that modern computers can't even evaluate what you should do, then you should probably quit playing that game.  But since you can't (since quitting the game would in and of itself count as a strategy), you might as well just choose randomly.  Believe me, whatever you choose doesn't matter.  They're all gonna suck.  

```r
choice_prob   <<- rep(1/length(attraction),length(attraction)) # 3
```

---

Lastly, we have this:

```r
attraction_prev <<- attraction
N_prev          <<- N
```

This one is fairly trivial.  These variables represent the values from last round, so we might as well set them at the end of this round.  We're gonna want them later.  

I should also take this opportunity to point out that we're using `<<-` instead of `<-` or `=` here.  `<<-` is the "non-local variable assignment operator" in R, which means we have to use it when we're setting variables outside of our current [scope](https://en.wikipedia.org/wiki/Scope_(computer_science)).  Normally this doesn't matter in R, because almost all values in R are passed [by value, not by reference](https://stackoverflow.com/questions/373419/whats-the-difference-between-passing-by-reference-vs-passing-by-value).  Reference classes, as would be expected by their name, work differently.  When we run things in a member method of a reference class, we want to be setting variables in the environment of the instance itself, not in the environment of the function.  If we don't use this special operator, we would create local variables in the function's environment, which will be de-referenced and garbage collected when that function finished computation.  

Long story short, when setting fields in a reference class, don't forget to use `<<-` and not `<-` or `=`; otherwise you won't change the values you're trying to.  

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

---

A note on hyperthreading as well.  You may wonder why the code now uses ```cl=makeCluster(detectCores(logical = FALSE))``` to determine how many cores to use.  Setting ```logical``` to ```FALSE``` will only create as many threads as there are physical cores, not logical cores.  For example, my Ryzen 5 1600X procesor has 6 physical cores, but each contains 2 logical cores, for a total of 12 logical cores.  These logical cores allow more operations to be done on a single physical core through a process called hyperthreading.  However the efficacy of this is heavily dependent on the type of operation being done, and it does not appear that R processes in general benefit from hyperthreading.  

https://www.r-bloggers.com/hyperthreading-ftw-testing-parallelization-performance-in-r/
https://heuristically.wordpress.com/2011/06/27/benchmarking-r-revolution-r-and-hyperthreading-for-data-mining/

So because of this, we save ourselves the headache of filling each logical core with a process, and just run one r process per physical core, as running on all logical cores is unnecessary and likely won't grant a significant performance difference.  