library(tidyverse)

setwd("/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/dsp_scanner/")

bhv_dat <- read_csv("data/clean/all_reward_output.csv") %>%
  group_by(subject) %>% mutate(trial = 1:n()) %>% ungroup() %>%
  mutate(rt = one_RT, 
         mt = movement_time, 
         targ = ifelse(sequence=='A',1,0),
         bhv_acc = accuracy,
         rew = reward,
         sub = subject) %>%
  dplyr::select(rt,mt,targ,bhv_acc,sub,rew,trial) %>%
  group_by(rt,mt,targ,bhv_acc,sub,rew) %>% filter(n() == 1)
  

dec_dat <- read_csv("data/clean/true_decoding_trials.csv") %>%
  mutate(tt = mt,  mt = tt - rt) %>%
  dplyr::select(rt,mt,targ,bhv_acc,sub,rew,dec_acc,roi)

all_dat <- tibble()
for (i in c("DLPFC","SMA","preSMA","M1","SPL","PMd")) {
  roi_dat <- dec_dat %>% filter(roi == i) %>%
    group_by(rt,mt,targ,bhv_acc,sub,rew,roi) %>% filter(n() == 1)
  tmp_dat <- bhv_dat %>% left_join(roi_dat, by = c("sub","rew","targ","rt","mt","bhv_acc"))
  print(nrow(tmp_dat))
  all_dat <- all_dat %>% bind_rows(tmp_dat)
}


plt_dat <- all_dat %>% group_by(sub,roi) %>% mutate(y0 = lag(bhv_acc)) %>% ungroup() %>%
  filter(!(is.na(y0) | is.na(roi)))

plt_dat %>% 
  filter(roi == "DLPFC") %>%
  ggplot(aes(x = factor(y0), y = bhv_acc,group=roi)) + 
  stat_summary(position=position_dodge(.2),geom="errorbar",aes(group=sub),alpha=.25) +
  stat_summary(position=position_dodge(.2),geom="line",aes(group=sub),alpha=.25) +
  stat_summary(size=1) +
  stat_summary(size=1,geom="line")

plt_dat %>% 
  ggplot(aes(x = factor(y0), y = dec_acc,group=roi)) + 
  stat_summary(position=position_dodge(.2),geom="errorbar",aes(group=sub),alpha=.25) +
  stat_summary(position=position_dodge(.2),geom="line",aes(group=sub),alpha=.25) +
  stat_summary(size=1) +
  stat_summary(size=1,geom="line") +
  facet_wrap(roi~.)

stat_dat <- plt_dat %>% group_by(sub,roi,y0) %>% 
  summarise(bhv_acc = mean(bhv_acc),dec_acc = mean(dec_acc)) %>% ungroup()

# BEHAVIOR
x = filter(stat_dat,roi == "DLPFC" & y0 == 1)$bhv_acc
y = filter(stat_dat,roi == "DLPFC" & y0 == 0)$bhv_acc
t.test(x,y,paired = TRUE)


# DECODING
x = filter(stat_dat,roi == "DLPFC" & y0 == 1)$dec_acc
y = filter(stat_dat,roi == "DLPFC" & y0 == 0)$dec_acc
t.test(x,y,paired = TRUE)

x = filter(stat_dat,roi == "SMA" & y0 == 1)$dec_acc
y = filter(stat_dat,roi == "SMA" & y0 == 0)$dec_acc
t.test(x,y,paired = TRUE)


x = filter(stat_dat,roi == "preSMA" & y0 == 1)$dec_acc
y = filter(stat_dat,roi == "preSMA" & y0 == 0)$dec_acc
t.test(x,y,paired = TRUE)

x = filter(stat_dat,roi == "PMd" & y0 == 1)$dec_acc
y = filter(stat_dat,roi == "PMd" & y0 == 0)$dec_acc
t.test(x,y,paired = TRUE)

x = filter(stat_dat,roi == "M1" & y0 == 1)$dec_acc
y = filter(stat_dat,roi == "M1" & y0 == 0)$dec_acc
t.test(x,y,paired = TRUE)

x = filter(stat_dat,roi == "SPL" & y0 == 1)$dec_acc
y = filter(stat_dat,roi == "SPL" & y0 == 0)$dec_acc
t.test(x,y,paired = TRUE)


