# Caleb Stone, July 2025: this script extracts, formats, and summarises data 
# from the working memory task of the 'doors' paradigm.  
rm(list=ls())

### load libraries
library(data.table)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(ggsci)

### settings
exp <- "exp-multi"

### set paths
exp_path <- str_glue("~/Documents/projects/doors_2025_analysis_multi/data/{exp}/")
# file_path <- file.path(exp_path)
if (!dir.exists(exp_path)) {
  stop(paste0(exp_path, " does not exist"))
}


### load behavioural data for wmt trials
pat_wmt <- '.*(mts_beh.tsv)'
files_wmt <- list.files(exp_path, pattern = pat_wmt, recursive = T)
dt_wmt <- rbindlist(lapply(file.path(exp_path, files_wmt), fread), fill = TRUE)

### add new variables
dt_wmt[, acc := case_when(resp == cresp ~ 1,  T ~ 0)] # if response == correct response, acc = 1 else acc = 0
dt_wmt[, cond := case_when((context == 1 | context == 2) ~ 'oc', T ~ 'nc')] # 'oc' = other context, 'nc' = neither context

### save data file
fln <- file.path("res", paste(paste(exp, "mts", sep = "_"), ".csv", sep = ""))
write_csv(dt_wmt, fln)

### create aggregate data frame
dt_wmt_avg <- dt_wmt[, 
                     .(accuracy_mean = mean(acc), 
                       rt_mean = mean(rt)),
                     by = .(sub, stage, cond)
]

### save data file
fln <- file.path("res", paste(paste(exp, "mts_avg", sep = "_"), ".csv", sep = ""))
write_csv(dt_wmt_avg, fln)

### calculate averages
# dt_wmt_avg[stage == 4, 
#            .(MeanAcc = mean(accuracy_mean),
#              MeanRT = mean(rt_mean)),
#            by = cond]

### plot
# label_sz <- 20
# mk_sz <- 2
# dt_wmt[stage == 3, 
#        .(MeanAcc = mean(acc),
#          MeanRT = mean(rt)),
#        by = .(sub, cond)] %>% 
#   ggplot(aes(x = cond, y = MeanAcc)) +
#   geom_violin() +
#   geom_point(position = position_jitter(width=.25)) + 
#   stat_summary(fun.data = "mean_cl_normal", 
#                geom = "pointrange", 
#                position = position_dodge(width = .9), 
#                linewidth = 1, 
#                size = mk_sz/2) +
#   theme_minimal() +
#   scale_x_discrete(labels = c("Neither", "Other ")) +
#   labs(title = "", x = "Context", y = "Proportion correct") +
#   theme(
#     plot.title = element_text(size = label_sz),
#     axis.text.x = element_text(size = label_sz), 
#     axis.text.y = element_text(size = label_sz), 
#     axis.title.x = element_text(size = label_sz), 
#     axis.title.y = element_text(size = label_sz), 
#     legend.title = element_text(size = label_sz),
#     legend.text = element_text(size = label_sz),
#   )

