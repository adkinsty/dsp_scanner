# Compares ROI sequence decoding to chance 

library(tidyverse)
library(ggdist)
library(ggthemes)

setwd('/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/dsp_scanner/')

dat <- read_csv("data/clean/all_decoding_summary.csv") #,col_types = "icciddddd") %>%
  filter(roi != "IPL") %>%
  mutate(rew = factor(rew,c(5,10,30)),
         roi = factor(roi,c("DLPFC","preSMA","SMA","PMd","M1","SPL")))

null_dat <- data %>% filter(rep > 0)
true_dat <- data %>% filter(rep == 0)

null_plt_dat <- null_dat %>%
  group_by(roi,rew,rep) %>% 
  summarise(ccr = mean(dec_acc))
null_ci <- null_plt_dat %>% group_by(roi,rew) %>% summarise(null_max = quantile(ccr,probs = 0.95))

true_plt_dat <- true_dat %>%
  mutate(ccr = dec_acc) %>%
  mutate(gmu = mean(ccr,na.rm=T)) %>%
  group_by(sub) %>% 
  mutate(smu = mean(ccr,na.rm=T)) %>% ungroup() %>%
  mutate(ccr_ = ccr - smu + gmu) %>%
  group_by(rew,roi) %>% 
  summarise(mean = mean(ccr,na.rm=T), se = sd(ccr_)/sqrt(n())) %>%
  inner_join(null_ci,by=c('rew','roi'))

pos = position_dodge(.5)

null_samples <- sort(sample(1:1000,100))
true_plt_dat %>%
  filter(roi %in% c("DLPFC","SMA","preSMA")) %>% 
  ggplot(aes(x=rew,y=mean,group=roi,colour=roi)) +
  geom_point(position=position_dodge(.1)) +
  geom_line(position=position_dodge(.1)) +
  geom_errorbar(position=position_dodge(.1),
                aes(ymax=mean+se,ymin=mean-se),width=0) +
  scale_x_discrete("Reward ($)") +
  scale_y_continuous("Mean Action Decoding") +  
  scale_colour_manual('ROI',values=c("black","#D55E00","#0072B2")) +
  facet_wrap(roi ~ .) +
  theme_tufte(base_size = 12,base_family = "sans") +
  theme(legend.background = element_blank(),
        legend.position = "none",
        axis.line = element_line())
ggsave("visuals/figures/raw/roi_rew_decoding.pdf",dpi=150,height=2.5,width=5)

true_dat %>%
  filter(roi %in% c("DLPFC","SMA","preSMA")) %>% 
  ggplot(aes(x=rew,y=dec_acc,group=sub,colour=roi)) +
  geom_point(position=position_dodge(.1),alpha=.75) +
  geom_line(position=position_dodge(.1),alpha=.75) +
  scale_x_discrete("Reward ($)") +
  scale_y_continuous("Action Decoding") +  
  scale_colour_manual('ROI',values=c("black","#D55E00","#0072B2")) +
  facet_wrap(roi ~ .) +
  theme_tufte(base_size = 12,base_family = "sans") +
  theme(legend.background = element_blank(),
        legend.position = "none",
        axis.line = element_line())
ggsave("visuals/figures/raw/sub_roi_rew_decoding.pdf",dpi=150,height=5,width=5)
