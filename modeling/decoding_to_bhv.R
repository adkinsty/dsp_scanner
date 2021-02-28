# analysis of trial-wise decoding results
setwd("/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/dsp_scanner/")

library(tidyverse)
dat <- read_csv("data/clean/true_decoding_trials.csv") %>% 
  filter(roi != "IPL") %>%
  mutate(seq = ifelse(targ == 0, -.5, .5),
         dec = ifelse(dec_acc == 0, -.5, .5))

sub_plt_dat <- dat %>%
  filter(roi %in% c("DLPFC","SMA","preSMA")) %>%
  mutate(y = bhv_acc) %>%
  group_by(sub,roi,dec_acc) %>%
  summarise(y = mean(y)) %>% ungroup()

plt_dat <- sub_plt_dat %>%
  mutate(gmu = mean(y,na.rm=T)) %>%
  group_by(sub) %>% 
  mutate(smu = mean(y,na.rm=T)) %>% ungroup() %>%
  mutate(y_ = y - smu + gmu) %>%
  group_by(roi,dec_acc) %>% 
  summarise(mean = mean(y,na.rm=T), se = sd(y_)/sqrt(n())) 

plt_dat %>%
  mutate(dec_acc = ifelse(dec_acc==1,"correct","incorrect"),
         dec_acc = factor(dec_acc,levels=c("incorrect","correct"))) %>%
  ggplot(aes(x=dec_acc,y=mean,ymax=mean+se,ymin=mean-se,colour=roi,group=roi)) +
  geom_point(position=position_dodge(.1)) +
  geom_line(position=position_dodge(.1)) +
  geom_errorbar(position=position_dodge(.1),width=0) +
  scale_x_discrete("Trial-wise Action Decoding") +
  scale_y_continuous("Behavioral Success") +  
  scale_colour_manual('ROI',values=c("black","#D55E00","#0072B2")) +
  facet_wrap(roi ~ .) +
  theme_tufte(base_size = 12,base_family = "sans") +
  theme(legend.background = element_blank(),
        legend.position = "none",
        axis.line = element_line())
ggsave("visuals/figures/raw/roi_decoding_bhv.pdf",dpi=150,height=2.5,width=5)


sub_plt_dat %>%
  ggplot(aes(x=factor(dec_acc),y=y,colour=roi,group=sub)) +
  geom_point(position=position_dodge(.1),alpha=.75) +
  geom_line(position=position_dodge(.1),alpha=.75) +
  scale_x_discrete("Action Decoding",labels=c("false","true")) +
  scale_y_continuous("Action success") +  
  scale_colour_manual('ROI',values=c("black","#D55E00","#0072B2")) +
  facet_wrap(roi ~ .) +
  theme_tufte(base_size = 12,base_family = "sans") +
  theme(legend.background = element_blank(),
        legend.position = "none",
        axis.line = element_line())
ggsave("visuals/figures/raw/sub_roi_decoding_bhv.pdf",dpi=150,height=5,width=5)

  

library(brms)
library(MASS)
options(mc.cores = parallel::detectCores())
# options(contrasts=rep("contr.treatment",2))
  
priors <- c(set_prior("normal(0,1)",class="b"),
            set_prior("normal(0,1)",class="Intercept"),
            set_prior("normal(0,1)",class="sd")) 

M1 <-  brm(bhv_acc ~ dec + seq +  (1|sub), filter(dat,roi=="M1"), 
           bernoulli(), priors, iter=5000, warmup=3000)
SMA <- update(M1, newdata = filter(dat,roi=='SMA'))
DLPFC <- update(M1, newdata = filter(dat,roi=='DLPFC'))
preSMA <- update(M1, newdata = filter(dat,roi=='preSMA'))
SPL <- update(M1, newdata = filter(dat,roi=='SPL'))
PMd <- update(M1, newdata = filter(dat,roi=='PMd'))

library(bayestestR)
describe_posterior(M1)
describe_posterior(SMA)
describe_posterior(DLPFC)
describe_posterior(preSMA)
describe_posterior(SPL)
describe_posterior(PMd)


# to check whether sequence decoding is better than (hacked) accuracy decoding
hack <- dat %>%
  filter(rew == 30) %>%
  group_by(sub,roi) %>%
  mutate(hack_acc = bhv_acc == dec_pred) %>%
  summarise(mean_hack = mean(hack_acc,na.rm=T),
            mean_dec = mean(dec_acc,na.rm=T)) %>%
  mutate(broke = mean_hack > mean_dec) %>%
  group_by(roi) %>%
  summarise(mean_hack = mean(mean_hack),
            mean_dec = mean(mean_dec),
            mean_broke = mean(broke))

