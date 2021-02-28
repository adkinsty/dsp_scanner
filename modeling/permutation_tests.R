library(tidyverse)

setwd("/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/dsp_scanner/")

dat <- read_csv("data/clean/all_decoding_summary.csv")

data_wide <- dat %>%
  group_by(roi,rep,rew) %>%
  summarise(dec_acc = mean(dec_acc)) %>% ungroup() %>%
  pivot_wider(id_cols = c(rep,roi), names_from = rew, 
              names_prefix = "r", values_from = dec_acc) %>%
  mutate(r30M5 = r30 - r5, r30M10 = r30 - r10) %>% filter(roi != "IPL")

true <- filter(data_wide,rep==0)
null <- filter(data_wide, rep!=0)

# 30 - 5 test
null %>% inner_join(true,by='roi',suffix=c("n","t")) %>%
  filter(r30M5n >= r30M5t) %>%
  group_by(roi) %>%
  summarise(mu = mean(r30M5t), C = n(), N = 1000, p = (C + 1) / (N + 1))
# 30 - 10 test
null %>% inner_join(true,by='roi',suffix=c("n","t")) %>%
  filter(r30M10n >= r30M10t) %>%
  group_by(roi) %>%
  summarise(mu = mean(r30M10t), C = n(), N = 1000, p = (C + 1) / (N + 1))

library(ggthemes)

null %>% 
  ggplot(aes(x=r30M5,group=roi)) +
  geom_histogram() +
  geom_vline(data=group_by(null,roi)%>%summarise(ci=quantile(r30M5,.95)),
             aes(xintercept=ci),colour="red3",size=1) +
  facet_wrap(roi ~ ., ncol = 3) +
  geom_vline(data=true,aes(xintercept=r30M5),colour="green3",size=1) +
  theme_tufte(base_size=10,base_family = "sans")
null %>% 
  ggplot(aes(x=r30M10,group=roi)) +
  geom_histogram() +
  geom_vline(data=group_by(null,roi)%>%summarise(ci=quantile(r30M10,.95)),
             aes(xintercept=ci),colour="red3",size=1) +
  facet_wrap(roi~.) +
  geom_vline(data=true,aes(xintercept=r30M10),colour="green3",size=1) +
  theme_tufte(base_size=10,base_family = "sans")

# 5
null %>% inner_join(true,by='roi',suffix=c("n","t")) %>%
  filter(r5n >= r5t) %>%
  group_by(roi) %>%
  summarise(mu = mean(r5t), C = n(), N = 1000, p = (C + 1) / (N + 1))
null %>% 
  ggplot(aes(x=r5,group=roi)) +
  geom_histogram() +
  geom_vline(data=group_by(null,roi)%>%summarise(ci=quantile(r5,.95)),
             aes(xintercept=ci),colour="red3",size=1) +
  facet_wrap(roi~.) +
  geom_vline(data=true,aes(xintercept=r5),colour="green3",size=1) +
  theme_tufte(base_size = 10,base_family = "sans")

# 10
null %>% inner_join(true,by='roi',suffix=c("n","t")) %>%
  filter(r10n >= r10t) %>%
  group_by(roi) %>%
  summarise(mu = mean(r10t), C = n(), N = 1000, p = (C + 1) / (N + 1))
null %>% 
  ggplot(aes(x=r10,group=roi)) +
  geom_histogram() +
  geom_vline(data=group_by(null,roi)%>%summarise(ci=quantile(r10,.95)),
             aes(xintercept=ci),colour="red3",size=1) +
  facet_wrap(roi~.) +
  geom_vline(data=true,aes(xintercept=r10),colour="green3",size=1)+
  theme_tufte(base_size = 10,base_family = "sans")

# 30
null %>% inner_join(true,by='roi',suffix=c("n","t")) %>%
  filter(r30n >= r30t) %>%
  group_by(roi) %>%
  summarise(mu = mean(r30t), C = n(), N = 1000, p = (C + 1) / (N + 1))
null %>% 
  ggplot(aes(x=r30,group=roi)) +
  geom_histogram() +
  geom_vline(data=group_by(null,roi)%>%summarise(ci=quantile(r30,.95)),
             aes(xintercept=ci),colour="red3",size=1) +
  facet_wrap(roi~.) +
  geom_vline(data=true,aes(xintercept=r30),colour="green3",size=1)+
  theme_tufte(base_size = 10,base_family = "sans")

  

