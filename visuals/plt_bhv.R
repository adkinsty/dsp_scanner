library(tidyverse)
library(plotrix)
library(ggthemes)

setwd("/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/dsp_scanner/")

data <- read_csv("data/clean/reward_loto_clf_bhv_data.csv") %>%
    filter(!is.na(dlpfc_clf_accs)) %>%
    mutate(
        seq=ifelse(sequence=="A","Trained","Novice"),
        bhv=accuracy,
        pfc=as.numeric(dlpfc_clf_accs),
        m1=as.numeric(m1_clf_accs),
        sma=as.numeric(sma_clf_accs),
        rew=as.factor(reward),
        sub=subject) %>%
    dplyr::select(c(seq,bhv,rew,sub,pfc,m1,sma,speed))

pos = position_dodge(.25)



f2Adata <-  data %>%
    mutate(bhv_mu = mean(bhv,na.rm=T)) %>%
    group_by(sub) %>% 
    mutate(bhv_id_mu = mean(bhv,na.rm=T)) %>% ungroup() %>%
    group_by(sub,rew) %>% 
    summarise(bhv = mean(bhv,na.rm=T), 
              bhv_id_mu = mean(bhv_id_mu,na.rm=T),
              bhv_mu=mean(bhv_mu,na.rm=T)) %>% 
    mutate(bhv_new = bhv - bhv_id_mu + bhv_mu) %>%
    group_by(rew) %>% 
    summarise(bhv = mean(bhv,na.rm=T),
              se=std.error(bhv_new,na.rm=T))
f2Adata %>%
    ggplot(aes(x=rew,y=bhv,ymin=bhv-se,ymax=bhv+se,group=1)) +
    geom_line(position=pos,size=1) +
    geom_point(position=pos,size=3) +
    geom_errorbar(position=pos,width=.1,size=.5) +
    scale_x_discrete("Reward ($)") +
    scale_y_continuous("Success Rate") +   
    theme_tufte(base_size = 12,base_family = "sans") +
    theme(legend.background = element_blank(),
          legend.position = "none",
          axis.line = element_line())
ggsave("visuals/figures/raw/f2A.pdf",units="in",height=3,width=3,dpi=150)

f2Bdata <-  data %>%
    mutate(speed_mu = mean(speed,na.rm=T)) %>%
    group_by(sub) %>% 
    mutate(speed_id_mu = mean(speed,na.rm=T)) %>% ungroup() %>%
    group_by(sub,rew) %>% 
    summarise(speed = mean(speed,na.rm=T), 
              speed_id_mu = mean(speed_id_mu,na.rm=T),
              speed_mu=mean(speed_mu,na.rm=T)) %>% 
    mutate(speed_new = speed - speed_id_mu + speed_mu) %>%
    group_by(rew) %>% 
    summarise(speed = mean(speed,na.rm=T),
              se=std.error(speed_new,na.rm=T))
f2Bdata %>%
    ggplot(aes(x=rew,y=speed,ymin=speed-se,ymax=speed+se,group=1)) +
    geom_line(position=pos,size=1) +
    geom_point(position=pos,size=3) +
    geom_errorbar(position=pos,width=.1,size=.5) +
    scale_x_discrete("Reward ($)") +
    scale_y_continuous("Speed (k/s)") +   
    theme_tufte(base_size = 12,base_family = "sans") +
    theme(legend.background = element_blank(),
          legend.position = "none",
          axis.line = element_line())
ggsave("visuals/figures/raw/f2B.pdf",units="in",height=3,width=3,dpi=150)


f2Adata <-  data %>%
    mutate(bhv_mu = mean(bhv,na.rm=T)) %>%
    group_by(sub) %>% 
    mutate(bhv_id_mu = mean(bhv,na.rm=T)) %>% ungroup() %>%
    group_by(sub,rew,seq) %>% 
    summarise(bhv = mean(bhv,na.rm=T), 
              bhv_id_mu = mean(bhv_id_mu,na.rm=T),
              bhv_mu=mean(bhv_mu,na.rm=T)) %>% 
    mutate(bhv_new = bhv - bhv_id_mu + bhv_mu) %>%
    group_by(rew,seq) %>% 
    summarise(bhv = mean(bhv,na.rm=T),
              se=std.error(bhv_new,na.rm=T))
f2Adata %>%
    ggplot(aes(x=rew,y=bhv,ymin=bhv-se,ymax=bhv+se,colour=seq,group=seq)) +
    geom_line(position=pos,size=1) +
    geom_point(position=pos,size=3) +
    geom_errorbar(position=pos,width=.1,size=.5) +
    scale_x_discrete("Reward ($)") +
    scale_y_continuous("Success Rate") +   
    scale_colour_brewer("Sequence",palette = "Paired") +
    theme_tufte(base_size = 12,base_family = "sans") +
    theme(legend.background = element_blank(),
          legend.position = "none",
          axis.line = element_line())
ggsave("visuals/figures/raw/f2A_seq.pdf",units="in",height=3,width=3,dpi=150)

f2Bdata <-  data %>%
    mutate(speed_mu = mean(speed,na.rm=T)) %>%
    group_by(sub) %>% 
    mutate(speed_id_mu = mean(speed,na.rm=T)) %>% ungroup() %>%
    group_by(sub,rew,seq) %>% 
    summarise(speed = mean(speed,na.rm=T), 
              speed_id_mu = mean(speed_id_mu,na.rm=T),
              speed_mu=mean(speed_mu,na.rm=T)) %>% 
    mutate(speed_new = speed - speed_id_mu + speed_mu) %>%
    group_by(rew,seq) %>% 
    summarise(speed = mean(speed,na.rm=T),
              se=std.error(speed_new,na.rm=T))
f2Bdata %>%
    ggplot(aes(x=rew,y=speed,ymin=speed-se,ymax=speed+se,colour=seq,group=seq)) +
    geom_line(position=pos,size=1) +
    geom_point(position=pos,size=3) +
    geom_errorbar(position=pos,width=.1,size=.5) +
    scale_x_discrete("Reward ($)") +
    scale_y_continuous("Speed (k/s)") +   
    scale_colour_brewer("Sequence",palette = "Paired") +
    theme_tufte(base_size = 12,base_family = "sans") +
    theme(legend.background = element_blank(),
          legend.position = "none",
          axis.line = element_line())
ggsave("visuals/figures/raw/f2B_seq.pdf",units="in",height=3,width=3,dpi=150)
