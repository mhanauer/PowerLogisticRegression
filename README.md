---
title: "Logit single level simulation"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Logistic regression single simulation 
SAS and R data management, statistical analysis, and graphics: https://gossetsstudent.wordpress.com/2010/11/18/logistic-regression-simulation-for-a-power-calculation/

Odds ratios are what we want: https://www.theanalysisfactor.com/why-use-odds-ratios/ 
Anything over or below 1 is a percentage change in the odds

# I think this saying if greater than some random generator then be a one if not, be a zero
Let us assume that we mean center the data, therefore, the mean is zero and intecept is the odds ratio at the mean of the assessment.

Two things that we need
1. Base odds of someone going back to jail (50/50?)
2. Percentage change in the odds (25% reduction?)

```{r}
intercept = log(1)
## 25% reduction in the odds of going back to jail
odds.ratio = .70
beta = log(odds.ratio)
n = 240
runs = 1000
# So this is how you get the random variation
xtest <- rnorm(n)
result <-  replicate(
              n = runs,
              expr = {
                  xtest <- rnorm(n)
                  linpred <- intercept + (xtest * beta)
                  prob <- exp(linpred)/(1 + exp(linpred))
                  runis <- runif(length(xtest),0,1)
                  ytest <- ifelse(runis < prob,1,0)
                  summary(model <- glm(ytest ~ xtest,  family = "binomial"))$coefficients[2,4] < .05
                  }
            )
power = sum(result)/runs; power
```
Run some bayes simulation and see how much data we would need
Library the packages
```{r}
library(MCMCpack)
library(descr)
library(ggplot2)
library(psych)
```
###################
Now try varying N's
###################
Want to make this about whether an effect is significantly different from zero or not.  So do the HDI's not include zero
So the only way that zero can be included is if the lower 2.5 is negative and the upper 97.5 is the positive.
```{r}
matt_power_sample = function(){
intercept = 0
### Might need a for loop here
n = list(150,160,170,180,190,210,220)
intervention_out = list()
treat1v0 =  log(.7)
y_dat = list()
prob = NULL
runis = NULL
y = NULL
dat_out = list()
for(i in 1:length(n)){
  intervention_out[[i]]= c(rep(1,round(n[[i]]*.5,1)), rep(0,round(n[[i]]*.5,0)))
  y_dat[[i]] = intercept + intervention_out[[i]]*treat1v0
  prob[[i]] <- exp(y_dat[[i]])/(1 + exp(y_dat[[i]]))
  runis[[i]] <- runif(length(prob[[i]]),0,1)
  y[[i]] <- ifelse(runis[[i]] < prob[[i]],1,0)
  dat_out[[i]] = data.frame(y = y[[i]], intervention = intervention_out[[i]])
}
## Now grab the 2.5 and 97.5 and then 
post_prior = list()
cred_inter_2.5 = list()
cred_inter_97.5 = list()
cred_inter_sum = list()
cred_inter_power = list()
for(i in 1:length(dat_out)){
post_prior[[i]] = MCMClogit(y ~ intervention,data = dat_out[[i]])
post_prior[[i]] = summary(post_prior[[i]])
cred_inter_2.5[[i]] = post_prior[[i]]$quantiles[2,c(1)]
cred_inter_97.5[[i]] = post_prior[[i]]$quantiles[2,c(5)]
cred_inter_2.5[[i]] = ifelse(cred_inter_2.5[[i]] < 0,1,0)
cred_inter_97.5[[i]] = ifelse(cred_inter_97.5[[i]] > 0,1,0)
cred_inter_sum[[i]] = sum(cred_inter_2.5[[i]], cred_inter_97.5[[i]])
cred_inter_power[[i]] = ifelse(cred_inter_sum[[i]] < 2, 1,0)
}
return(cred_inter_power)
}

```
Now try to rep the function
```{r}
reps = 100
power_rep = replicate(reps, matt_power_sample())
power_unlist= unlist(power_rep)
n = c(150,160,170,180,190,210,220)
power_matrix = matrix(power_unlist, ncol = reps, nrow = length(n), byrow = FALSE)
power = apply(power_matrix, 1, sum)/reps
power
```
Now get the graph for power at different effect sizes
```{r}
sample_size = unlist(n)
power_dat = data.frame(sample_size, power)
power_dat
plot(power_dat, main = "Power with effect size of .2")
```
