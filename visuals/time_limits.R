
setwd("/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/dsp_scanner/")

data <- read_csv("data/clean/all_reward_output.csv") %>%
  group_by(subject,sequence) %>%
  mutate(trial = 1:n()) %>% ungroup() %>%
  dplyr::select(trial,subject,sequence,max_time)

plt_data <- data %>%
  group_by(trial,sequence,subject) %>%
  summarise(y = mean(max_time))
  

data %>%
  ggplot(aes(x=trial,y=max_time,group=sequence,colour=sequence)) +
  geom_line() +
  facet_wrap(subject ~ .) +
  theme_classic() 

