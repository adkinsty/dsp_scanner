# to demonstrate learning

library(tidyverse)
library(fs)
library(stringr)

setwd('/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/dsp_scanner/')

data_dir <- "data/raw/dsp/"
files <- dir_ls(path = data_dir,regexp =  "[0-9]_training_output.csv")
dat <- tibble()
for (fn in files) {
  dat <- read_tsv(fn) %>% 
    bind_rows(dat)
}

data <- dat %>% 
  mutate(x = str_remove_all(pressed,"\\[|\\]"),
         x = str_remove_all(x," "),
         keys_pressed = str_remove_all(x,","),
         nkey = nchar(x)) %>%
  filter(movement_time > 0 & nkey > 0) %>%
  mutate(tt = (movement_time + one_IKI)/1000, kps = nkey / tt,
         seq = sequence, sub = subject) 

data %>%
  group_by(sub,seq) %>%
  mutate(trial = 1:n()) %>% ungroup() %>%
  group_by(seq,trial) %>%
  summarise(mu = mean(kps), se = sd(kps)/sqrt(n())) %>%
  ggplot(aes(x=trial,y=mu,ymin=mu-se,ymax=mu+se,colour=seq,group=seq)) +
  geom_point() + 
  geom_smooth(se=F)


data %>%
  filter(accuracy == 1) %>%
  filter(block %in% c(1, 8)) %>%
  mutate(blk = factor(block,c(1,8))) %>%
  group_by(sub,seq,blk) %>%
  summarise(kps = mean(kps)) %>% 
  ggplot(aes(y=kps,x=seq,group=interaction(blk,seq),colour=blk,fill=blk)) +
  geom_violin(colour=NA,scale = "width") +
  geom_point(position=position_dodge(.9),colour="black",alpha=.25)+
  stat_summary(position=position_dodge(.9),colour="black",size=1,fun.data = "mean_cl_boot") +
  scale_fill_brewer("Block",palette = "Set1") +
  scale_y_continuous("Speed (k/s)") +
  scale_x_discrete("Sequence",labels=c("Trained","Novice","Random")) +
  theme_grey(base_family="sans",base_size=15) +
  theme(legend.background = element_blank(),
        legend.position = c(.8,.8))
  


  

  
