# Caleb Stone, July 2025: this scripts reformats and combines some data files 
# for later analysis in jamovi. Run this script after run_wrangling, run_wmt, and run_survey

### load libraries
library(data.table)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(ggsci)
library(reshape2)

### exp-flex
exp <- "exp-flex"

# load data files
avg_flex <- fread("res/exp-flex_avg.csv")
svy_flex <- fread("res/exp-flex_svy.csv")

# fill in missing data - if no setting errors, replace NaN values with 0 for setting sticks/slips
avg_flex[setting_errors_mean == 0, ':=' (setting_sticks_mean = 0,
                                         setting_slips_mean = 0)]

# calculate linear integrated speed-accuracy score (LISAS) # Vandierendonck (2017), Behav Res, 49:653–673 
avg_flex[, ':=' (setting_sticks_PE = setting_sticks_mean * setting_errors_mean, # calculate setting_sticks as a proportion of total errors
                 setting_slips_PE = setting_slips_mean * setting_errors_mean) # calculate setting_slips as a proportion of total errors
][, ':=' (rt_first_correct_mean_SD = sd(rt_first_correct_mean),  # calculate standard deviation across switch condition by subject and session for variables of interest
          rt_subs_correct_mean_SD = sd(rt_subs_correct_mean),
          rt_post_error_mean_SD = sd(rt_post_error_mean),
          setting_sticks_PE_SD = sd(setting_sticks_PE),
          setting_slips_PE_SD = sd(setting_slips_PE)), 
  by=.(sub, ses)
][, ':=' (LISAS_rt_fst_cor = rt_first_correct_mean + (setting_sticks_PE*(rt_first_correct_mean_SD/setting_sticks_PE_SD)), 
          LISAS_rt_sub_cor = rt_subs_correct_mean + (setting_slips_PE*(rt_subs_correct_mean_SD/setting_slips_PE_SD)))] # compute LISAS variables

# reshape data
avg_flex_wide <- dcast.data.table(avg_flex, 
                                  sub + ses + train_type ~ switch, 
                                  value.var = names(avg_flex)[5:21]) # long to wide

# add survey data
svy_flex_totals <- svy_flex[, .(sub, total_wout_observing, total_w_observing)]
avg_flex_wide <- svy_flex_totals[avg_flex_wide, on='sub'] 

# save output file
fln <- file.path("res", paste(paste(exp, "avg-wide", sep = "_"), ".csv", sep = ""))
write_csv(avg_flex_wide, fln)


### exp-multi
exp <- "exp-multi"

# load data files
avg_multi <- fread("res/exp-multi_avg.csv")
mts_multi <- fread('res/exp-multi_mts_avg.csv')

# fill in missing data - if no setting errors, replace NaN values with 0 for setting sticks/slips
# avg_multi[setting_errors_mean == 0, ':=' (setting_sticks_mean = 0,
#                                           setting_slips_mean = 0)]

# reshape search task data
dc1 <- dcast.data.table(avg_multi, 
                 sub + train_type + ses ~ switch, 
                 value.var = names(avg_multi)[7:14], 
                 subset = .(ses==2)) # long to wide for training session
dc2 <- dcast.data.table(avg_multi, 
                 sub + train_type + ses + multi_trial ~ multi_cond, 
                 value.var = names(avg_multi)[7:14], 
                 subset = .(ses==3)) # long to wide for test session
avg_multi_wide <- rbindlist(list(dc1, dc2), fill=T) # combine into 1 data frame

# save data file
fln <- file.path("res", paste(paste(exp, "avg-wide", sep = "_"), ".csv", sep = ""))
write_csv(avg_multi_wide, fln)

# reshape working memory task data

avg_multi_mts_wide <- dcast.data.table(mts_multi,
                                       sub + stage ~ cond,
                                       value.var = c('accuracy_mean', 'rt_mean'),
                                       fun.aggregate=mean) # long to wide for working memory task data
avg_multi_mts_wide <- avg_multi_mts_wide[dc1[, .(sub, train_type)], on='sub'] # get train_type information
setnames(avg_multi_mts_wide, 'stage', 'ses') # rename stage to ses 

# save wokring memory data file
fln <- file.path("res", paste(paste(exp, "mts_avg-wide", sep = "_"), ".csv", sep = ""))
write_csv(avg_multi_mts_wide, fln)
