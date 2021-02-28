library(tidyverse)

setwd('/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/dsp_scanner/')

dat <- read_csv("data/clean/all_decoding_summary.csv") %>%
  filter(roi %in% c("DLPFC", "SMA", "preSMA", "PMd", "M1", "SPL"))

dat_wide <- dat %>%
  pivot_wider(id_cols = c(rep,roi,sub), names_from = rew, 
              names_prefix = "r", values_from = dec_acc) %>%
  mutate(r30M5 = r30 - r5, r30M10 = r30 - r10) %>%
  group_by(roi, rep) %>%
  summarise_at(vars(contains("r")), mean) %>% ungroup()

null <- dat_wide %>% filter(rep > 0)
truth <- dat_wide %>% filter(rep == 0)

N <- 1e3

r5counts <- c()
for (i in 1:N) {
  tmp1 <- null %>% group_by(roi) %>% summarise(x = sample(r5, size = 1))
  tmp2 <- null %>% group_by(roi) %>% summarise(ci = quantile(r5, .95)) %>% ungroup() %>%
    mutate(obs = tmp1$x) %>%
    mutate(sig = obs >= ci)
  r5counts <- append(r5counts, sum(tmp2$sig))
}
tibble(x = r5counts) %>% group_by(x) %>% summarise(p = n() / N) %>%
  ggplot(aes(x = x, y = p)) + geom_col() + 
  geom_text(aes(label = p), position = position_nudge(y = .02)) +
  scale_x_continuous("Number of ROIs with p < .05", breaks = c(0, 1, 2, 3, 4, 5, 6), limits = c(-.5, 6)) +
  scale_y_continuous("Null bootstrap proportion", breaks = seq(0, 1, .1)) 


r10counts <- c()
for (i in 1:N) {
  tmp1 <- null %>% group_by(roi) %>% summarise(x = sample(r10, size = 1))
  tmp2 <- null %>% group_by(roi) %>% summarise(ci = quantile(r10, .95)) %>% ungroup() %>%
    mutate(obs = tmp1$x) %>%
    mutate(sig = obs >= ci)
  r10counts <- append(r10counts, sum(tmp2$sig))
}
tibble(x = r10counts) %>% group_by(x) %>% summarise(p = n() / N) %>%
  ggplot(aes(x = x, y = p)) + geom_col() + 
  scale_x_continuous("Number of ROIs with p < .05", breaks = c(0, 1, 2, 3, 4, 5, 6), limits = c(-.5, 6)) +
  scale_y_continuous("Null bootstrap proportion", breaks = seq(0, 1, .1))


r30counts <- c()
for (i in 1:N) {
  tmp1 <- null %>% group_by(roi) %>% summarise(x = sample(r30, size = 1))
  tmp2 <- null %>% group_by(roi) %>% summarise(ci = quantile(r30, .95)) %>% ungroup() %>%
    mutate(obs = tmp1$x) %>%
    mutate(sig = obs >= ci)
  r30counts <- append(r30counts, sum(tmp2$sig))
}
tibble(x = r30counts) %>% group_by(x) %>% summarise(p = n() / N) %>%
  ggplot(aes(x = x, y = p)) + geom_col() + 
  geom_text(aes(label = p), position = position_nudge(y = .02)) +
  scale_x_continuous("Number of ROIs with p < .05", breaks = c(0, 1, 2, 3, 4, 5, 6), limits = c(-.5, 6)) +
  scale_y_continuous("Null bootstrap proportion", breaks = seq(0, 1, .1))



r30M5counts <- c()
for (i in 1:N) {
  tmp1 <- null %>% group_by(roi) %>% summarise(x = sample(r30M5, size = 1))
  tmp2 <- null %>% group_by(roi) %>% summarise(ci = quantile(r30M5, .95)) %>% ungroup() %>%
    mutate(obs = tmp1$x) %>%s
    mutate(sig = obs >= ci)
  r30M5counts <- append(r30M5counts, sum(tmp2$sig))
}
tibble(x = r30M5counts) %>% group_by(x) %>% summarise(p = n() / N) %>%
  ggplot(aes(x = x, y = p)) + geom_col() + 
  geom_text(aes(label = p), position = position_nudge(y = .02)) +
  scale_x_continuous("Number of ROIs with p < .05", breaks = c(0, 1, 2, 3, 4, 5, 6), limits = c(-.5, 6)) +
  scale_y_continuous("Null bootstrap proportion", breaks = seq(0, 1, .1))


r30M10counts <- c()
for (i in 1:N) {
  tmp1 <- null %>% group_by(roi) %>% summarise(x = sample(r30M10, size = 1))
  tmp2 <- null %>% group_by(roi) %>% summarise(ci = quantile(r30M10, .95)) %>% ungroup() %>%
    mutate(obs = tmp1$x) %>%
    mutate(sig = obs >= ci)
  r30M10counts <- append(r30M10counts, sum(tmp2$sig))
}
tibble(x = r30M10counts) %>% group_by(x) %>% summarise(p = n() / N) %>%
  ggplot(aes(x = x, y = p)) + geom_col() + 
  scale_x_continuous("Number of ROIs with p < .05", breaks = c(0, 1, 2, 3, 4, 5, 6), limits = c(-.5, 6)) +
  scale_y_continuous("Null bootstrap proportion", breaks = seq(0, 1, .1))



N <- 1e4
counts <- c()
for (i in 1:N) {
  fake_rois <- sample(null$roi, replace = FALSE)
  tmp <- null %>% mutate(roi = fake_rois) %>%
    group_by(roi) %>%
    summarise(ci = quantile(r30, .95)) %>% ungroup() %>%
    mutate(obs = truth$r30) %>%
    mutate(sig = obs >= ci)
  counts <- append(counts, sum(tmp$sig))
}
counts %>% qplot()


truth %>% 
  ggplot() + 
  geom_histogram(data = null, aes(x = r30)) +
  geom_vline(data = null, aes(xintercept = quantile(r30,.95)), colour = "red") +
  geom_vline(aes(xintercept = r30), colour = "green") +
  facet_wrap(roi ~ .)

truth %>% 
  ggplot() + 
  geom_histogram(data = null %>% 
    filter(roi %in% c("DLPFC", "SMA", "M1")) %>%
    group_by(rep) %>% summarise(max_stat = max(r30)),
                 aes(x = max_stat)) +
  geom_vline(aes(xintercept = r30), colour = "green") +
  facet_wrap(roi ~ .)



