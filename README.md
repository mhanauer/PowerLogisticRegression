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

Need to develop a loop.
```{r}
intercept = log(1.5)
odds.ratio = log(.78)
n = 100
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






