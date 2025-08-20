get_rts <- function(data, experiment, multi_data){
  
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
           rt = off-start) %>%
    ungroup()

  # now depending on the experiment, filter rts by subject, 
  # now for each ses and switch condition, filter first correct and subs correct separately

  if (experiment == 'exp-flex'){
    
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
      filter(rt <= upper | is.na(upper)) # added |is.na(upper) to prevent cases when upper can't be calculated

    # now summarise
    data <- data %>% group_by(sub, ses, subses, switch, t, context, train_type) %>%
      summarise(
        rt_first_correct = mean(rt[rt_idx == 'first_correct'], na.rm=T), 
        rt_subs_correct = mean(rt[rt_idx == 'subs_correct'], na.rm=T), 
        rt_post_error = mean(rt[rt_idx == 'post_error'], na.rm=T),
        .groups='drop')
    
    
  } else {
   
     data <- join_multi_data(data, multi_data)
     rt_max <- 3 # anything more than this we believe is not real (KG eyeballed histogram of all data collected by July 4th 2025)
     sd_crit = 2.5
    
     data <- data %>% filter(rt < rt_max)
     
     # going to split the data into sessions 1 & 2 and treat them differently, 
     # as there are different conditions to filter
     # now create a summary of mu and sd rt for each rt type, for sessions 1 and 2
     learn_train <- data %>% filter(ses < 3)
     learn_train_sum <- learn_train %>% group_by(sub, ses, switch, rt_idx) %>%
       summarise(
         mu_rt = mean(rt, na.rm=T), 
         sd_rt = sd(rt, na.rm=T), 
         n = n(), 
         .groups='drop') 
     learn_train <- inner_join(learn_train, learn_train_sum %>% mutate(upper = mu_rt + (sd_crit*sd_rt)), 
                               by=c('sub', 'ses', 'switch', 'rt_idx')) %>%
       filter(rt <= upper) 
     
     
     test <- data %>% filter(ses == 3)
     test_sum <- test %>% group_by(sub, ses, rt_idx, multi_trial, multi_cond) %>%
       summarise(
         mu_rt = mean(rt, na.rm=T), 
         sd_rt = sd(rt, na.rm=T), 
         n = n(), 
         .groups='drop') 
     test <- test %>% inner_join(test_sum %>% mutate(upper = mu_rt + (sd_crit*sd_rt)),
                                 by=c('sub', 'ses', 'rt_idx', 'multi_trial', 'multi_cond')) %>%
       filter(rt <= upper)
     
      data <- rbind(learn_train, test)
      
      # now summarise
      data <- data %>% group_by(sub, ses, subses, switch, t, context, train_type, multi_trial,
                        multi_cond) %>%
        summarise(
          rt_first_correct = mean(rt[rt_idx == 'first_correct'], na.rm=T), 
          rt_subs_correct = mean(rt[rt_idx == 'subs_correct'], na.rm=T), 
          rt_post_error = mean(rt[rt_idx == 'post_error'], na.rm=T),
          .groups='drop')
    
  } 
  
  return(data)
  
} 