# lydia barnes, august 2024
# if someone is deep into a set of doors, they may struggle to respond to a cue to change contexts.
# this could be independent from, or anti-correlated with, how likely they are to make a setting error later in a trial
# both of these things are bundled into setting_errors. this function separates the two.

# K.G. amending the code, as we don't have to worry about the task jumps measure in 2025
# simplifying it to idx oc errors according to if they were made before the first correct response (stick),
# or if they were made after the first correct response (slip). This is done trial by trial, for
# aggregation later

get_setting_stability <- function(data){

  data <- data %>%
    group_by(sub,ses,t) %>%
    mutate(
      first_cc = which(door_cc>0)[1], # this gets you the trial number of the first correct click
      set_err_idx = case_when(
        row_number() < first_cc & door_oc > 0 ~ 'stick', 
        row_number() > first_cc & door_oc > 0 ~ 'slip', .default = NA)
      ) %>%
    ungroup()

  return(data)
}

