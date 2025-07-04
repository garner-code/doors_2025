join_multi_data <- function(data, multi_data){
  # K. Garner - 2025
  # this joins together the grp level search task data with the multitask conditions
  
  # what I ultimately want is a vector that says - 'first' response of 
  # multitasking block, or 'subsequent' response of multitasking block
  # and one that says - other, neither, or none to denote multitasking condition
  # these vectors should be taken from multidata, but joined to data, by sub, ses, and trial number
  multi_data <- multi_data %>% mutate(multi_trial = case_when(mem_tgt_trial > 0 ~ 'first', .default ='subsequent'),
                                      multi_cond = case_when(mem_context == 1 | mem_context == 2 ~ 'other',
                                                             mem_context == 3 | mem_context == 4 ~ 'neither',
                                                             .default='none')) %>% 
    rename(ses=sess) %>%
    select(sub, ses, t, context, multi_trial, multi_cond)
  
  data <- left_join(data, multi_data, by=c('sub', 'ses', 't', 'context'))
  return(data)
}