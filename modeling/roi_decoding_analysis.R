# analysis of trial-wise decoding results
setwd("/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/dsp_scanner/")

library(tidyverse)
data <- read_csv("data/clean/true_decoding_trials.csv") %>%
  filter(roi %in% c("preSMA","SPL","IPL","PMd")) %>%
  mutate(y = dec_acc, x = factor(rew, c(30, 10, 5)), z = roi, j = sub)

library(brms)
library(MASS)
options(mc.cores = parallel::detectCores())
options(contrasts=rep("contr.treatment",2))
  
priors <- c(set_prior("normal(0,1)",class="b"),
            set_prior("normal(0,1)",class="Intercept"),
            set_prior("normal(0,1)",class="sd")) 

pSMA <- brm(y ~ x + (1|j), filter(data,z=="preSMA"), bernoulli(), priors)
SPL <- update(pSMA, newdata = filter(data,roi=='SPL'))
IPL <- update(pSMA, newdata = filter(data,roi=='IPL'))
PMd <- update(pSMA, newdata = filter(data,roi=='PMd'))


library(bayestestR)
describe_posterior(pSMA)
describe_posterior(SPL)
describe_posterior(IPL)
describe_posterior(PMd)


