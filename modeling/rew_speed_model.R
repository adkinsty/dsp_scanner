library(tidyverse)
library(brms)
library(sjstats)
library(bayesplot)

options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

run_model <- function(expr, path, reuse = TRUE) {
  path <- paste0(path, ".Rds")
  if (reuse) {
    fit <- suppressWarnings(try(readRDS(path), silent = TRUE))
  } else {
    fit <- eval(expr)
    saveRDS(fit, file = path)
  }
  fit
}
rescale.center = function(x,m2,s2) {
  # rescales x to have mean of m2 and sd of s2
  # http://www.stat.columbia.edu/~gelman/research/unpublished/standardizing.pdf
  # http://www.stat.columbia.edu/~gelman/research/published/priors11.pdf
  m1 = mean(x,na.rm=T)
  s1 = sd(x,na.rm=T)
  y = m2 + (x - m1) * (s2/s1)
  return(y)
}
mean_blk = mean(c(1,2,3,4,5,6,7,8))

setwd("/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/dsp_scanner/")

data = read_csv("data/clean/reward_brain_bhv_data.csv") %>% 
  mutate(n_OG = n()) %>% # count total observations before filtering
  # replace zero speed with NaN
  mutate(speed = ifelse(speed == 0,NaN,speed)) %>%
  # fix colnames, do some preproc
  mutate(rew = factor(reward,levels=c("5","10","30")),
         blk = (block-mean_blk)/10,
         time = rescale.center(max_time,0,0.5),
         slow = too_slow,
         acc = accuracy,
         ker = error,
         spd = rescale.center(speed,0,0.5),
         pfc = rescale.center(dlpfc_betas,0,0.5),
         m1 = rescale.center(m1_betas,0,0.5),
         sma = rescale.center(sma_betas,0,0.5),
         seq = factor(sequence,levels=c("B","A")),
         sub = factor(subject)) %>%
  mutate(n_new = n_OG - n()) %>%
  dplyr::select(acc,rew,seq,blk,sub,pfc,m1,sma,spd,speed,block,ker,slow,
         trial_num,acc,mt,time,seq_keys,pressed,n_keys,n_new)

# PRIORS
priors = c(set_prior("normal(0,1)",class="Intercept"),
           set_prior("normal(0,1)",class="b"),
           set_prior("normal(0,1)",class="sd"))

# MODEL 
model = run_model(
  expr = brm(
    formula = 
      spd ~ 1 + rew*seq + (1 | sub),
    data = data,
    family = gaussian(), # https://cran.r-project.org/web/packages/brms/vignettes/brms_families.html
    prior = priors, 
    warmup = 4500, iter = 5000, chains = 4,seed = 420), 
  path = "rew_speed_brms",
  reuse = FALSE)
