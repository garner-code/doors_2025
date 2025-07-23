# Caleb Stone, July 2025: this script extracts, formats, and summarises data 
# from Five Facet Mindfulness questionnaire for the 'doors' paradigm.  
# Scoring information for scale found at https://link.springer.com/rwe/10.1007/978-3-030-77644-2_15-1

### load libraries
library(data.table)
library(tidyverse)
library(magrittr)

### settings
exp <- 'exp-flex' #'exp-multi' #'exp-flex'

### set paths
exp_path <- str_glue("data/{exp}/")
# file_path <- file.path(exp_path)
if (!dir.exists(exp_path)) {
  stop(paste0(exp_path, " does not exist"))
}

### create scoring variables
# items to be reverse scored
reverse_score <- c('Q12','Q16','Q22','Q5','Q8','Q13','Q18','Q23','Q28','Q34',
                   'Q38','Q3','Q10','Q14','Q17','Q25','Q30','Q35','Q39') 
# sub-scales
observing <- c('Q1','Q6','Q11','Q15','Q20','Q26','Q31','Q36')
describing <- c('Q2','Q7','Q12','Q16','Q22','Q27','Q32','Q37')
acting_w_awareness <- c('Q5','Q8','Q13','Q18','Q23','Q28','Q34','Q38')
nonjudging <- c('Q3','Q10','Q14','Q17','Q25','Q30','Q35','Q39')
nonreactivity <- c('Q4','Q9','Q19','Q21','Q24','Q29','Q33')

### load survey data
dt_ffmq <- fread(exp_path + 'FFMQ.csv')

### clean data
dt_ffmq %<>% 
  rename(sub = SUBID_1) %>%
  rename_with(~ paste0("Q", seq(1,39)), starts_with("FFMQ")) %>%
  filter(Finished == 'True' & sub %in% seq(1,100)) %>% 
  select(c(sub, Q1:Q39))

# cols <- c('sub', paste0('FFMQ_B1_', seq(1,20)), paste0('FFMQ_B2_', seq(1,12)))
# dt_ffmq <- dt_ffmq[Finished == 'True' & sub %in% seq(1,100), ..cols]

# convert text labels to numeric scores for all response variables
dt_ffmq %<>% mutate(
  across(
    all_of(reverse_score), ~ # reverse score relevant questions
    case_when( 
      . == "Never or very rarely true" ~ 5,
      . == "Rarely true" ~ 4,
      . == "Sometimes true" ~ 3,
      . == "Often true" ~ 2,
      . == "Very often or always true" ~ 1
      )
    )
  ) %>% mutate(
  across(
    -all_of(c(reverse_score, 'sub')), ~ # score everything else 
      case_when( 
        . == "Never or very rarely true" ~ 1,
        . == "Rarely true" ~ 2,
        . == "Sometimes true" ~ 3,
        . == "Often true" ~ 4,
        . == "Very often or always true" ~ 5
      )
    )
  )

### calculate total scores for each sub-scale and whole scale 
dt_ffmq %<>% rowwise() %>% 
  mutate(
    observing = sum(across(all_of(observing))),
    describing = sum(across(all_of(describing))),
    acting_w_awareness = sum(across(all_of(acting_w_awareness))),
    nonjudging = sum(across(all_of(nonjudging))),
    nonreactivity = sum(across(all_of(nonreactivity))),
    total_w_observing = sum(across(starts_with('Q'))),
    total = total_w_observing - sum(across(all_of(observing))) 
)

### save data file
fln <- file.path("res", paste(paste(exp, "svy", sep = "_"), ".csv", sep = ""))
write_csv(dt_ffmq, fln)

            

