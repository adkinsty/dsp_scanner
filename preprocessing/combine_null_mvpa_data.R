library(tidyverse)
library(stringr)
library(purrr)
library(fs)

setwd('/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/dsp_scanner/')

data_dir <- "data/raw/brain/mvpa/permutation_results/"
paths <- dir_ls(data_dir,type = "file")

trials_path <- "data/clean/all_decoding_trials.csv"
if (file.exists(trials_path)) file.remove(trials_path)
summ_path <- "data/clean/all_decoding_summary.csv"
if (file.exists(summ_path)) file.remove(summ_path)
truth_path <- "data/clean/true_decoding_trials.csv"
if (file.exists(truth_path)) file.remove(truth_path)

na_mean <- function(x) {
  return(mean(x,na.rm=T))
}

header <- TRUE
for (tmp_path in paths) {
  print(tmp_path)
  
  trials <- read_csv(tmp_path) %>%
    mutate(filename = str_replace(tmp_path,data_dir,""),
           sub_roi = str_replace(filename,".csv","")) %>%
    separate(sub_roi,c("sub","roi"))
  write_csv(trials,trials_path, append = TRUE,col_names = header)
  
  summ <- trials %>% 
    group_by(rep,roi,rew,sub) %>%
    summarise_at(c("bhv_acc","dec_pred","dec_acc","mt","rt"),na_mean)
  write_csv(summ,summ_path, append = TRUE,col_names = header)
  
  truth <- trials %>% filter(rep == 0)
  write_csv(truth,truth_path, append = TRUE,col_names = header)
  header <- FALSE
}
