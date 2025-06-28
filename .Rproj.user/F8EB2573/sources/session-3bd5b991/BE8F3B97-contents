# lydia barnes, august 2024
# if someone is deep into a set of doors, they may struggle to respond to a cue to change contexts.
# this could be independent from, or anti-correlated with, how likely they are to make a setting error later in a trial
# both of these things are bundled into setting_errors. this function separates the two.

# K.G. amending the code, as we don't have to worry about the task jumps measure in 2025
# simplifying it to get the number of oc errors made before the first correct response,
# and how many made after the 1st correct response, on each trial

get_idx_first_cc <- function(trial_dat){
  with(trial_dat, min(which(door_cc > 0)))
}

get_n_obs_for_trial <- function(trial_dat){
  nrow(trial_dat)
}

mrk_sticks_vs_slips_for_trial <- function(trial_dat){
  trial_dat$sticks <- 0
  trial_dat$slips <- 0
  first_correct <- get_idx_first_cc(trial_dat)
  max_events <- get_n_obs_for_trial
  if (first_correct > 1){
      trial_dat$sticks[1:(first_correct-1)] <- trial_dat$door_oc[1:(first_correct-1)]
      trial_dat$slips[first_correct:max_events] <- trial_dat$door_oc[first_correct:max_events]
  }
  trial_dat
}



get_setting_stability <- function(data){

  data <- 
  # data <- data %>%
  #   group_by(sub,ses,t) %>%
  #   mutate(
  #     stick_idx =
  #     scca = case_when(diff(c(1,door_cc))>0~1,.default=0), #
  #     sccb = case_when(diff(c(0,door_cc))>0~1,.default=0), #
  #     select_cc = case_when(ses==2 & switch==1 ~ sccb, .default=scca), #ediately
  #     soca = case_when(diff(c(0,door_oc))>0~1,.default=0), # this does the opposite to the above - if you start the trial hitting doors from the other context then put a 1
  #     socb = case_when(diff(c(1,door_oc))>0~1,.default=0), #
  #     select_oc = case_when(ses==2 & switch==1 ~ socb, .default=soca), #changes into other context
  #     select_oc_late = case_when(diff(c(0,t))==1~0,.default=select_oc) # if it is the first trial put a 0, if not put select_oc
  #   ) %>%
  #   ungroup()
  # 
  # x <- data %>%
  #   group_by(sub,ses,t) %>%
  #   mutate(
  #     t_cc = cumsum(door_cc),
  #     t_oc = cumsum(door_oc)
  #     ) %>%
  #   ungroup()

  return(data)
}

