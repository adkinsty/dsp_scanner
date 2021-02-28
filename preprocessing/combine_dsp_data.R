#Script to create a master file of behavioral data from the all subjects 
library(data.table)
wd = "/Users/adkinsty/Box Sync/LeeLab/Experiments/data/DSP_scanner/output_files/"
setwd(wd)
#Make a list of all subject numbers
subNums = c(104,105,106,107,108,109,110,111,
            112,114,115,116,117,118,119,123,124,125,126,
            128,129,130,131,132,133,134,135,136,137,138) 

all_reward_data = data.table()
#all_training_data = data.table()

#gather info from every excel file into a masterfile
for (subNum in subNums) { 
  reward_file = sprintf("%s_reward_output.csv",subNum)
  reward_data = fread(reward_file)
  
  #training_file = sprintf("%s_training_output.csv",subNum)
  #training_data = fread(training_file)
  
  all_reward_data = rbind(all_reward_data, reward_data, fill = TRUE)
  #all_training_data = rbind(all_training_data, training_data, fill = TRUE)
  
}
write.table(all_reward_data, file = sprintf("%s/../output_files/all_reward_output.csv", wd), 
            sep = ",", row.names = FALSE)
# write.table(all_training_data, file = sprintf("%s/../output_files/all_training_output.csv", wd), 
#             sep = ",", row.names = FALSE)

# all_correct_train_data = all_training_data %>% filter(accuracy==1)
# 
# write.table(all_correct_train_data, file = sprintf("%s/../output_files/all_correct_training_output.csv", wd), 
#             sep = ",", row.names = FALSE)
