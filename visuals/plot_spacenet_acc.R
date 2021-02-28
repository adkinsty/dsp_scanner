library(tidyverse)
library(viridis)

setwd("/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/dsp_scanner/")

data = read_csv("data/raw/brain/mvpa/old/all_spacenet_accuracies.csv") %>%
    mutate(Target = ifelse(targets == "seq","Sequence", "Performance"),
           Target = factor(Target,levels = c("Sequence","Performance")))

df1 <-  data %>% filter(targets == "seq")
t.test(df1$acc,mu = 0.5)
df2 <-  data %>% filter(targets == "acc")
t.test(df2$acc,mu = 0.5)

df1 %>%
    ggplot(aes(y=acc,x=Target)) +
    geom_violin(colour="#ed9a25",fill="#ed9a25",alpha=.25,size=1) +
    geom_point(aes(group=sub),position=position_dodge(.1),colour="grey") +
    stat_summary(fun.data = "mean_cl_boot",size=1) +
    geom_hline(yintercept=0.5,linetype="dashed") +
    scale_y_continuous("Action decoding") +
    theme_tufte(base_family="sans",base_size=12) +
    theme(axis.line.y = element_line(size=.5),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank()) 
ggsave("visuals/figures/raw/spacenet_seq_decoding.pdf",height=3,width=2,dpi=150)

df2 %>%
    ggplot(aes(y=acc,x=Target)) +
    geom_violin(colour="#b4dfed",fill="#b4dfed",alpha=.25,size=1) +
    geom_point(aes(group=sub),position=position_dodge(.1),colour="grey") +
    stat_summary(fun.data = "mean_cl_boot",size=1) +
    geom_hline(yintercept=0.5,linetype="dashed") +
    scale_y_continuous("Performance decoding") +
    theme_tufte(base_family="sans",base_size=12) +
    theme(axis.line.y = element_line(size=.5),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank()) 
ggsave("visuals/figures/raw/spacenet_acc_decoding.pdf",height=3,width=2,dpi=150)

sub_ids <- unique(data$sub)
N_sub <- length(sub_ids)

N_rep <- 1000
reps <- 1:N_rep

t_seq <- array(data=vector(),dim=N_rep)
t_acc <- array(data=vector(),dim=N_rep)
for (i in reps) {
    print(i)
    ids <- sample(sub_ids,N,replace=T)
    x_seq <- array(data=vector(),dim=N_sub)
    x_acc <- array(data=vector(),dim=N_sub)
    for (j in 1:N) {
        x_acc[j] <- filter(data,sub==ids[j] & targets == 'acc')$acc
        x_seq[j] <- filter(data,sub==ids[j] & targets == 'seq')$acc
    }
    t_seq[i] <- t.test(x_seq,mu=0.5)$estimate
    t_acc[i] <- t.test(x_acc,mu=0.5)$estimate
}
plt_dat <- tibble(dec = c(t_seq,t_acc), 
                  i = c(reps,reps), 
                  t = c(rep('action',N_rep),
                        rep('performance',N_rep)))

plt_dat %>%
    ggplot(aes(x=dec, fill=t)) +
    geom_histogram(colour="white",binwidth = .01) +
    scale_fill_manual("Decoding\nTarget",values=c("#ed9a25","#b4dfed")) +
    scale_y_continuous("Bootstrap replications") +
    scale_x_continuous("Decoding accuracy",limits=c(0.5,.85)) +
    theme_tufte(base_family="sans",base_size=12) +
    theme(axis.line = element_line(size=0.5))
ggsave("visuals/figures/raw/boostrap_distributions.pdf",height=2,width=6,dpi=150)


