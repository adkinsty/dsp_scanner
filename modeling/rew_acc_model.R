library(brms)
library(rstantools)
library(dplyr)
library(data.table)
library(rstan)
library(lme4)
library(tidybayes)
library(tidyr)
#library(modelr::)
library(bayesplot)
#library(emg)
library(sjPlot)
#library(brmstools)
library(plotrix)
library(sjstats)

run_model <- function(expr, path, reuse = TRUE) {
    path <- paste0(path, ".Rds")
    if (reuse) {
        fit <- suppressWarnings(try(readRDS(path), silent = TRUE))
    } else {
        fit <- eval(expr)
        saveRDS(fit, file = path)
    }
    fit
}
rescale.center = function(x,m2,s2) {
    # rescales x to have mean of m2 and sd of s2
    # http://www.stat.columbia.edu/~gelman/research/unpublished/standardizing.pdf
    # http://www.stat.columbia.edu/~gelman/research/published/priors11.pdf
    m1 = mean(x,na.rm=T)
    s1 = sd(x,na.rm=T)
    y = m2 + (x - m1) * (s2/s1)
    return(y)
}
get_rope = function(x,outcome) {
    if (outcome %in% c("mt","movement_time","MT")) {
        rope = 10 / (2*sd(x))
    } else if (outcome %in% c("rt","one_RT", "reaction_time")) {
        rope = 5 / (2*sd(x))
    } else if (outcome %in% c("acc","accuracy","ACC")) { # accuracy and retention
        rope = 0.05
    } else if (outcome %in% c("ret","RET","retention")) {
        rope = 0.01 / (2*sd(x))
    } else {
        rope = 0
    }
    return(rope)
}
logit2prob <- function(logit){
    odds <- exp(logit)
    prob <- odds / (1 + odds)
    return(prob)
}

setwd("/Users/adkinsty/Box/LeeLab/Experiments/data/DSP_scanner/analysis_scripts/tyler/bayesian/")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

################################################
# acc ###########################################
################################################

mean_blk = mean(c(1,2,3,4,5,6,7,8))

data = fread("reward_brain_bhv_data.csv") %>% 
    mutate(n_OG = n()) %>% # count total observations before filtering
    # replace zero speed with NaN
    mutate(speed = ifelse(speed == 0,NaN,speed)) %>%
    # fix colnames, do some preproc
    mutate(rew = factor(reward,levels=c("5","10","30")),
           blk = (block-mean_blk)/10,
           time = rescale.center(max_time,0,0.5),
           slow = too_slow,
           acc = accuracy,
           ker = error,
           spd = rescale.center(speed,0,0.5),
           pfc = rescale.center(dlpfc_betas,0,0.5),
           m1 = rescale.center(m1_betas,0,0.5),
           sma = rescale.center(sma_betas,0,0.5),
           seq = factor(sequence,levels=c("B","A")),
           sub = factor(subject)) %>%
    mutate(n_new = n_OG - n()) %>%
    select(acc,rew,seq,blk,sub,pfc,m1,sma,spd,speed,block,ker,slow,
           trial_num,acc,mt,time,seq_keys,pressed,n_keys,n_new)



# PRIORS
priors = c(set_prior("normal(0,1)",class="Intercept"),
           set_prior("normal(0,1)",class="b"),
           set_prior("normal(0,1)",class="sd"))

# MODEL 
model = run_model(
    expr = brm(
        formula = 
            acc ~ 1 + rew + seq + rew:seq + (1 | sub),
        data = data,
        family = bernoulli(), # https://cran.r-project.org/web/packages/brms/vignettes/brms_families.html
        prior = priors, 
        warmup = 5000, iter = 10000, chains = 4,seed = 420), 
    path = "rew_acc_brms",
    reuse = TRUE)

summary(model)
tidy_stan(model,prob = 0.95)
tidy_summ_acc = tidy_stan(model,prob = 0.90)
sub_rew_eff_acc = tidy_summ_acc$`random.conditional.rew30: sub`$Estimate



rope = get_rope(data$acc,"acc")
rope(model, rope = c(-rope,rope),prob=0.9)
equi_test(model,rope=c(-rope,rope))


data %>% 
    group_by(sub, rew,seq) %>% summarise(acc = mean(acc,na.rm=T)) %>% ungroup() %>%
    group_by(rew,seq) %>% summarise(se = std.error(acc,na.rm=T),
                                acc = mean(acc,na.rm=T)) %>%
    ggplot(aes(x=rew,y=acc,colour=seq,group=seq)) + 
    geom_point(size=2.5,position=position_dodge(.2)) + 
    geom_line(size=1,position=position_dodge(.2)) + 
    geom_errorbar(aes(ymax=acc+se,ymin=acc-se),
                  position=position_dodge(.2),
                  size=1,
                  width=0.3,alpha=0.75) +
    scale_colour_manual(values=c("#86B3E3","#248EFB")) + #values=c("#0b5b94","#d2e1eb")
    scale_x_discrete("", labels=c("$5","$10","$30")) +
    scale_y_continuous("",limits=c(.32,.52)) +
    theme_classic(base_family = "Helvetica", base_size = 20) +
    theme(legend.position = "none",
          axis.title = element_blank()) 
ggsave(filename="figures/acc_rew.pdf",
       dpi=300,units="in",height=3,width=3)




# posterior draws for later use
posterior = as.array(model)
head(posterior)

# get NUTS parameters for later use
np <- nuts_params(model)
head(np)

# organize parameter names
parameters = get_variables(model)
b_pars = c(parameters[1:9])[-c(1,6)]
sd_pars = c(parameters[16:22])


coef.logodds <- model %>% gather_draws(b_rew30,b_pfc,b_m1) %>% median_hdi()
print(exp(coef.logodds[c(".value",".lower",".upper")])) 


# posterior intervals
# FIXED EFFECTS
color_scheme_set("blue")
intervals.b = mcmc_intervals(posterior, prob=0.50,prob_outer=0.95,
                             pars = b_pars)
intervals.b = intervals.b + 
    geom_vline(xintercept = get_rope(data$acc,"acc"),colour="red",linetype="dashed") + 
    geom_vline(xintercept = -get_rope(data$acc,"acc"),colour="red",linetype="dashed") + 
    scale_x_continuous("Posterior") +
    scale_y_discrete("Parameter",
                     labels = c(
                         "b_Intercept"="Intercept",
                         "b_rew10"="$10-$5","b_rew30"="$30-$5",
                         "b_blk"="Block",
                         "b_seqA"="Heavy-Light","b_pfc"="DLPFC",
                         "b_m1"="M1","b_sma"="SMA","b_time"="Time-Limit"))
ggsave(plot=intervals.b,"figures/acc_FixedEf_intervals.png",units="in",height=8,width=6)

##########################
# USED IN POSTER
##########################
color_scheme_set("blue")
areas.b = mcmc_areas(posterior, prob=0.90,prob_outer=1,
                     pars = c("b_rew30"))
areas.b = areas.b + 
    scale_x_continuous("") + 
    geom_vline(xintercept = 0, 
               linetype = "dashed",
               color = "red", 
               size = 1.5) +
    theme_classic(base_size = 18,base_family = "Helvetica") + 
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())

ggsave(plot=areas.b,"figures/acc_5_30_FixedEf_area.pdf",units="in",height=3,width=3)
############################
############################

color_scheme_set("blue")
areas.b = mcmc_areas(posterior, prob=0.90,prob_outer=1,
                     pars = c("b_rew10"))
areas.b = areas.b + 
    scale_x_continuous("") + 
    geom_vline(xintercept = 0, 
               linetype = "dashed",
               color = "red", 
               size = 1.5) +
    theme_classic(base_size = 18,base_family = "Helvetica") + 
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
ggsave(plot=areas.b,"figures/acc_5_10_FixedEf_area.pdf",units="in",height=3,width=3)



 # interaction
color_scheme_set("blue")
areas.b = mcmc_areas(posterior, prob=0.90,prob_outer=1,
                     pars = c("b_rew30:seqA"))
areas.b + 
    scale_x_continuous("") + 
    geom_vline(xintercept = 0, 
               linetype = "dashed",
               color = "red", 
               size = 1.5) +
    theme_classic(base_size = 18,base_family = "Helvetica") + 
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
ggsave("figures/acc_5_30_seqA_FixedEf_area.pdf",units="in",height=3,width=3)




color_scheme_set("blue")
intervals.sd = mcmc_intervals(posterior, prob=0.50,prob_outer=0.95,
                              pars = sd_pars)
intervals.sd = intervals.sd + 
    scale_x_continuous("Credible Intervals") #+
ggsave(plot=intervals.sd,"figures/acc_sd_intervals.png",units="in",height=8,width=6)



# check for collinearity (narrow bivariate) and "multiplicative non-identifiables" (bannana-like)
color_scheme_set("blue")
pairs.b.main = mcmc_pairs(posterior, np = np, 
                          off_diag_args = list(size = 0.1),
                          pars = c("b_blk","b_seqB","b_time"))
ggsave(plot=pairs.b.main,"figures/acc_time_mcmc_pairs.png",units="in",height=10,width=10)





# check for collinearity (narrow bivariate) and "multiplicative non-identifiables" (bannana-like)
color_scheme_set("blue")
pairs.b.main = mcmc_pairs(posterior, np = np, 
                          off_diag_args = list(size = 0.1),
                          pars = c("b_rew10","b_rew30","b_seqB","b_blk","b_m1","b_pfc"))
ggsave(plot=pairs.b.main,"figures/acc_mainFixedEf_mcmc_pairs.png",units="in",height=10,width=10)

# trace plots
# fixed effects
color_scheme_set("mix-blue-gray")
trace.b.inter = mcmc_trace(posterior, 
                           pars = b_pars, 
                           np = np,facet_args = c(ncol=4)) +
    xlab("Post-warmup iteration")
ggsave(plot=trace.b.inter,"figures/acc_FixedEf_traces.png",units="in",height=6,width=12)

# random effects
color_scheme_set("mix-blue-gray")
trace.sd = mcmc_trace(posterior, 
                      pars = sd_pars, 
                      np = np, facet_args = c(ncol=3)) +
    xlab("Post-warmup iteration") 
ggsave(plot=trace.sd,"figures/acc_subSD_traces.png",units="in",height=4,width=8)





                                                                                                                                                                  
theme_mine <- function(base_size = 18, base_family = "Helvetica") {
    # Starts with theme_grey and then modify some parts
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(
            strip.background = element_blank(),
            strip.text.x = element_text(size = 18),
            strip.text.y = element_text(size = 18),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14,hjust=1),
            axis.ticks =  element_line(colour = "black"), 
            axis.title.x= element_text(size=16),
            axis.title.y= element_text(size=16,angle=90),
            panel.background = element_blank(), 
            panel.border =element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            #panel.spacing = unit(1.0, "lines"), 
            plot.background = element_blank(), 
            #plot.spacing = unit(c(0.5,  0.5, 0.5, 0.5), "lines"),
            axis.line.x = element_line(color="black", size = 1),
            axis.line.y = element_line(color="black", size = 1)
        )
}



# posterior predictive check
accs = data$acc
y.acc = accs #accs[!is.na(accs)]
yrep.acc = posterior_predict(model,nsamples=4000)


loo1 = brms::loo(model,)

ppc_loo_intervals(y.acc,yrep.acc,loo1)

# density match?
color_scheme_set("viridis")
ppc.dens = ppc_dens_overlay(y.acc, yrep.acc[1:100, ]) + 
    scale_x_continuous("Accuracy") + scale_y_continuous("Density")
ggsave(plot=ppc.dens,"figures/acc_ppc_density.png",units="in",height=3,width=4)

# recover mean?
color_scheme_set("blue") 
ppc.mean = ppc_stat(y.acc,yrep.acc,stat="mean") + 
    scale_x_continuous("") + 
    theme_classic(base_size = 18,base_family = "Helvetica") + 
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          strip.background = element_blank(),
          legend.position = "none",
          axis.line.x = element_line(color="black", size = 1),
          axis.line.y = element_line(color="black", size = 1))
ggsave(plot=ppc.mean,"figures/acc_ppc_mean.pdf",units="in",height=3,width=3)

# recover sd?
ppc.sd = ppc_stat(y.acc,yrep.acc,stat="sd") +
    scale_x_continuous("",limits=c(0.49,0.5),breaks=c(0.49,0.50)) + 
    theme_classic(base_size = 18,base_family = "Helvetica") + 
    theme(#axis.text.y=element_blank(),
        #axis.ticks.y=element_blank(),
        strip.background = element_blank(),
        legend.position = "none",
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))
ggsave(plot=ppc.sd,"figures/acc_ppc_sd.pdf",units="in",height=3,width=3)

# recover mean foreach rew ?
color_scheme_set("blue")
data$rew = factor(data$rew,levels=c('$5',"$10","$30"))
ppc_stat_grouped(y.acc,yrep.acc,group = data$rew,
                            stat="mean",binwidth = .005) + 
    scale_x_continuous("") + 
    scale_y_continuous("",breaks=c(100,300,500)) +
    facet_wrap(facets=.~group, nrow = 3) +
    theme_classic(base_size = 18,base_family = "Helvetica") + 
    theme(#axis.text.y=element_blank(),
          #axis.ticks.y=element_blank(),
          strip.background = element_blank(),
          legend.position = "none",
          axis.line.x = element_line(color="black", size = 1),
          axis.line.y = element_line(color="black", size = 1))
ggsave("figures/acc_ppc_rew_mean.pdf",units="in",height=6.5,width=3.5)

# recover sd foreach rew ?
color_scheme_set("blue")
levels(data$rew) = c("$5","$10","$30")
ppc.mean = ppc_stat_grouped(y.acc,yrep.acc,group = data$rew,
                            stat="sd",binwidth = .005) + 
    scale_x_continuous("") + 
    facet_wrap(.~group,scales = "fixed") +
    theme_classic(base_size = 18,base_family = "Helvetica") + 
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          strip.background = element_blank(),
          legend.position = "none",
          axis.line.x = element_line(color="black", size = 1),
          axis.line.y = element_line(color="black", size = 1))
ggsave(plot=ppc.mean,"figures/acc_ppc_rew_sd.pdf",units="in",height=4,width=10)

# recover mean foreach seq ?
color_scheme_set("blue")
ppc.mean = ppc_stat_grouped(y.acc,yrep.acc,stat="mean",group = data$seq) + 
    scale_x_continuous("Mean of Accuracy") + scale_y_continuous("Frequency") + facet_wrap(.~group,nrow=3) +
    theme(legend.text = element_text(size=10), legend.title = element_text(size=10))
ggsave(plot=ppc.mean,"figures/acc_ppc_seq_mean.png",units="in",height=3,width=3)

# recover sd foreach seq?
color_scheme_set("blue")
ppc.sd = ppc_stat_grouped(y.acc,yrep.acc,stat="sd",group = data$seq) + 
    scale_x_continuous("SD of Accuracy",breaks = c(0.47,0.48,0.49,0.50)) + scale_y_continuous("Frequency") + facet_wrap(.~group,nrow=3) +
    theme(legend.text = element_text(size=10), legend.title = element_text(size=10))
ggsave(plot=ppc.sd,"figures/acc_ppc_seq_sd.png",units="in",height=3,width=3)




















# marginal effects
rew_cond <- data.frame(seq = c("A","B"),time=c(1e-10),blk=c(1e-10),
                       pfc=c(1e-10),sma=c(1e-10),m1=c(1e-10))
rew_me = marginal_effects(model, effects = "rew", 
                          conditions = rew_cond)
alph=0.9
dodg=0.
rew_plt = rew_me$rew %>% 
    ggplot(aes(x=rew,y=estimate__,group=seq,colour=seq)) + 
    geom_point(position=position_dodge(dodg),alpha=alph) + 
    # geom_errorbar(
    #     aes(ymax=upper__,ymin=+lower__),
    #     width=0.1,position=position_dodge(dodg),alpha=alph) +
    geom_line(position=position_dodge(dodg),alpha=alph) +
    scale_x_discrete("Incentive", labels=c("$5","$10","$30")) +
    scale_y_continuous("Success Rate") +
    scale_color_manual("Training",labels=c("Light", "Heavy"),values=c("#E69F00", "#56B4E9")) +
    theme_classic(base_family = "Times New Roman")
ggsave(filename="figures/acc_rew_me.png",plot=rew_plt,units="in",height=3,width=3)


seq_cond <- data.frame(time=c(1e-10),blk=c(1e-10),
                       rew = c("5","10","30"),
                       pfc=c(1e-10),sma=c(1e-10),m1=c(1e-10))
seq_me = marginal_effects(model, effects   = "seq", points=TRUE, 
                          method = "fitted",
                          conditions = seq_cond)
alph=0.9
dodg=0.3
seq_plt = seq_me$seq %>% 
    ggplot(aes(x=seq,y=estimate__,colour=rew,group=rew)) + 
    geom_point(position=position_dodge(dodg),alpha=alph) + 
    geom_errorbar(
        aes(ymax=upper__,ymin=+lower__),
        width=0.1,position=position_dodge(dodg),alpha=alph) +
    geom_line(position=position_dodge(dodg),alpha=alph) +
    scale_x_discrete("Training", labels=c("Heavy","Light")) +
    scale_y_continuous("Success Rate") +
    scale_color_manual("Incentive",labels=c("$5","$10","$30"),values=c("#F0E442", "#0072B2", "#D55E00")) +
    theme_classic(base_family = "Times New Roman")  
ggsave(filename="figures/acc_seq_me.png",plot=seq_plt,units="in",height=3,width=4)



blk_cond <- data.frame(rew = c("10"),seq=c("A"),time=c(1e-10),
                       pfc=c(1e-10),sma=c(1e-10),m1=c(1e-10))
blk_me = marginal_effects(model, effects = "blk", 
                          conditions = blk_cond)
blk_plt = blk_me$blk %>%
    ggplot(aes(x=blk,y=estimate__)) + 
    geom_line() +
    geom_ribbon(aes(ymin=lower__,ymax=upper__),alpha=0.3,color="grey") +
    scale_x_continuous("Block ") +
    scale_y_continuous("Success Rate") +
    theme_classic(base_family = "Times New Roman")  
ggsave(filename="figures/acc_blk_me.png",plot=blk_plt,units="in",height=3,width=3)


time_cond <- data.frame(rew = c("10"),seq=c("A"),blk=c(1e-10),
                        pfc=c(1e-10),sma=c(1e-10),m1=c(1e-10))
time_me = marginal_effects(model, effects = "time", 
                           conditions = time_cond)
time_plt = time_me$time %>%    
    ggplot(aes(x=time,y=estimate__)) + 
    geom_line() +
    geom_ribbon(aes(ymin=lower__,ymax=upper__),alpha=0.3) +
    scale_x_continuous("Time Limit ") +
    scale_y_continuous("Success Rate") +
    theme_classic(base_family = "Times New Roman")  
ggsave(filename="figures/acc_time_me.png",plot=time_plt,units="in",height=3,width=3)


pfc_cond <- data.frame(rew = c("10"),seq=c("A"),blk=c(1e-10),
                       time=c(1e-10),sma=c(1e-10),m1=c(1e-10))
pfc_me = marginal_effects(model, effects = "pfc", 
                          conditions = pfc_cond)
pfc_plt = pfc_me$pfc %>%    
    ggplot(aes(x=pfc,y=estimate__)) + 
    geom_line() +
    geom_ribbon(aes(ymin=lower__,ymax=upper__),alpha=0.3) +
    scale_x_continuous("DLPFC Activity ") +
    scale_y_continuous("Success Rate") +
    theme_classic(base_family = "Times New Roman")  
ggsave(filename="figures/acc_pfc_me.png",plot=pfc_plt,units="in",height=3,width=3)


sma_cond <- data.frame(rew = c("10"),seq=c("A"),blk=c(1e-10),
                       pfc=c(1e-10),time=c(1e-10),m1=c(1e-10))
sma_me = marginal_effects(model, effects = "sma", 
                          conditions = sma_cond)
sma_plt = sma_me$sma %>%
    ggplot(aes(x=sma,y=estimate__)) + 
    geom_line() +
    geom_ribbon(aes(ymin=lower__,ymax=upper__),alpha=0.3) +
    scale_x_continuous("SMA Activity ") +
    scale_y_continuous("Success Rate") +
    theme_classic(base_family = "Times New Roman")  
ggsave(filename="figures/acc_sma_me.png",plot=sma_plt,units="in",height=3,width=3)


m1_cond <- data.frame(rew = c("10"),seq=c("A"),blk=c(1e-10),
                      pfc=c(1e-10),sma=c(1e-10),time=c(1e-10))
m1_me = marginal_effects(model, effects = "m1", 
                         conditions = m1_cond)
m1_plt = m1_me$m1 %>%
    ggplot(aes(x=m1,y=estimate__)) + 
    geom_line() +
    geom_ribbon(aes(ymin=lower__,ymax=upper__),alpha=0.3) +
    scale_x_continuous("M1 Activity ") +
    scale_y_continuous("Success Rate") +
    theme_classic(base_family = "Times New Roman")  
ggsave(filename="figures/acc_m1_me.png",plot=m1_plt,units="in",height=3,width=3)




