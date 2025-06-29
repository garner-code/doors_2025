get_rts <- function(data, experiment){
  
  # for each participant, I will first code all their rt's as either first correct (the first response on the trial was correct,
  # and should be included in the 'first rt' measure, or as subsequently correct.
  # if you make an error in your proceeding move, then the trial is coded as 'post-error'.
  data <- data %>% group_by(sub, ses, t) %>%
    mutate(rt_idx = case_when( 
              row_number() == 1 & door_cc == 1 ~ 'first_correct', 
              row_number() > 1 & door_cc == 1 & lag(door_cc) == 1 ~ 'subs_correct', 
              row_number() > 1  & door_cc == 1 & lag(door_cc) == 0 ~ 'post_error', .default=NA),
           prev_off = lag(off),
           prev_off = ifelse(row_number()==1, 0, prev_off),
           rt = on-prev_off) %>%
    ungroup()

  # now depending on the experiment, filter rts by subject, 
  # now for each ses and switch condition, filter first correct and subs correct separately
  if (experiment == 'exp-flex'){
    
    # first, get the number of trials in the dataframe
    
    rt_max = 2.5
    sd_crit = 2.5
    data <- data %>% filter(rt < rt_max)
    
    # now create a summary of mu and sd rt for each rt type, for each condition
    data <- data %>% filter(!is.na(rt_idx)) 
    data_sum <- data %>% group_by(sub, ses, switch, rt_idx) %>%
      summarise(
        mu_rt = mean(rt, na.rm=T), 
        sd_rt = sd(rt, na.rm=T), 
        n = n(), 
        .groups='drop') 
    data <- inner_join(data, data_sum %>% mutate(upper = mu_rt + (sd_crit*sd_rt)), by=c('sub', 'ses', 'switch', 'rt_idx')) %>%
      filter(rt <= upper) 
    
    # now summarise
    data <- data %>% group_by(sub, ses, subses, switch, t, context, train_type) %>%
      summarise(
        rt_first_correct = mean(rt[rt_idx == 'first_correct'], na.rm=T), 
        rt_subs_correct = mean(rt[rt_idx == 'subs_correct'], na.rm=T), 
        rt_post_error = mean(rt[rt_idx == 'post_error'], na.rm=T),
        .groups='drop')
    
    
  } else {
  
    # multitasking cleaning will go here 
    
  } 
  
  return(data)
  
} 