### Equivalence of Rasch and MLM ###
########### Zhaowen Guo ###########

# prepare packages and data
library(lme4)
library(optimx)
library(mirt)
library(psych)
library(tidyverse)
lsat <- read.csv("lsat.csv",header=TRUE)

## Rasch ----
# estimate item parameters
rasch <- mirt(data  = lsat,
              model = 1,
              itemtype = "Rasch",
              SE = TRUE)

rasch_coef <- coef(rasch, IRTpars=TRUE, simplify=TRUE)
rasch_items <- data.frame(rasch_coef$items)
rasch_items
# a: discrimination parameter of all items is set to 1 
# g: guessing parameter c is set to 0
# b: difficulty parameter
# interpretation: according to the model estimation, a person with ability level that is 0.24 standard deviation 
# below the average has 50% of chance to answer the item Q3 correctly.

itemfit(rasch, fit_stats = "S_X2")
# nonsignificant p-values indicate that the model fit the data

M2(rasch)
# RMSEA < .06; SRMSR < .08; CFI > .95; TLI > .95

residuals(rasch, df.p = T)
# check local independence assumption (large and significant LD indices is a red flag)

# estimate person parameters (ability estimates)
rasch_fs <- fscores(rasch, full.scores.SE = TRUE)

## Multilevel modeling ----
# change the default control setting to aid model convergence
control = glmerControl(optimizer = "optimx", 
                       calc.derivs = FALSE, 
                       optCtrl = list(method = "nlminb", 
                                      starttests = FALSE, 
                                      kkt = FALSE))

# reshape the data into a long format 
lsat_long <- lsat %>%
  mutate(ID = row_number()) %>%
  pivot_longer(!ID, names_to = "items", values_to = "responses")

# fit the model 
mlm <- glmer(responses ~ -1 + items + (1|ID), 
             family = "binomial", 
             data = lsat_long, 
             control = control)
# -1 (or 0) indicates that we don’t want an intercept in our estimation
summary(mlm)

## Results comparison ----
# compare item estimates
coef(summary(mlm))[,1]
rasch_items$b
cor(coef(summary(mlm))[,1], rasch_items$b) # -1

# compare person estimates
unlist(ranef(mlm))
fscores(rasch)
cor(unlist(ranef(mlm)), fscores(rasch)) # 1

