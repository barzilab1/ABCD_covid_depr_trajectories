# library(dplyr)
# library(readr)
# library(tidyr)
# library(missForest)
# library(lubridate)
# library(janitor)

# source("data organization/data_utility_fun.R")

# Get pre-covid data (choose 1 closet timepoint to covid (Mar 2020 to Jul 2021))
get_pre_post_covid_tp <- function(abcd_general_data) {

  abcd_general_data <- abcd_general_data %>%
    mutate(
      before_covid = if_else(interview_date < as.Date("2020-03-01"), 1, 0, missing = NA_real_),
      after_covid = if_else(interview_date >= as.Date("2021-08-01"), 1, 0, missing = NA_real_),
      timepoint = case_when(
        substr(eventname, 1, 1) == "b" ~ 0,
        TRUE ~ as.numeric(substr(eventname, 1, 1))
      )
    )

  # Choose 1 closet timepoint pre-covid
  abcd_general_pre_covid_tp <- abcd_general_data %>%
    filter(before_covid == 1) %>%
    group_by(src_subject_id) %>%
    # Select the row with the maximum 'timepoint' for each subject
    slice_max(timepoint, n = 1, with_ties = FALSE) %>%
    mutate(timepoint = 0)

  abcd_general_post_covid_tp <- abcd_general_data %>%
    filter(after_covid == 1) %>%
    group_by(src_subject_id) %>%
    # Select the row with the minimun 'timepoint' for each subject
    slice_min(timepoint, n = 1, with_ties = FALSE) %>%
    mutate(timepoint = 5)

  abcd_general_pre_post_covid_tp <- abcd_general_pre_covid_tp %>% bind_rows(abcd_general_post_covid_tp)

  return(abcd_general_pre_post_covid_tp)
}



impute_main_study_data <- function(merged_data){

  ##################################################
  ##################################################
  # BASED ON MISSING DATA, NEED TO IMPUTE DATA SEPARATELY AMONG TIMEPOINTS
  ## BASELINE: KSADS
  ## 1Y: BPM, POA
  ## 2Y: BPM, MCTQ, KSADS
  ## 3Y: BPM, POA, MCTQ
  ## 4Y: BPM, MCTQ, KSADS

  vars_abcd <- c("ksads_1_1_t_br", "ksads_1_2_t_br", "ksads_1_3_t_br", "ksads_1_4_t_br", "ksads_1_5_t_br", "ksads_1_6_t_br",
                 "ksads_1_179_t_br", "ksads_1_180_t_br", "ksads_1_159_t_br", "ksads_1_160_t_br", "ksads_1_181_t_br",
                 "ksads_1_182_t_br", "ksads_1_177_t_br", "ksads_1_178_t_br", "ksads_1_163_t_br", "ksads_1_164_t_br",
                 "bpm_18_y", "bpm_12_y", "bpm_9_y", "bpm_4_y",
                 "poa_nihtb_1_y", "poa_nihtb_6_y", "poa_nihtb_8_y", "poa_nihtb_9_y",
                 "mctq_sd_min_to_sleep_calc", "mctq_fd_min_to_sleep_calc", # impute this in order to calculate mctq_sfd_min_to_sleep_br # better to impute original variables
                 "mctq_sd_num_wake_up", "mctq_fd_num_wake_up" # For mctq_sfd_num_wake_up_br and mctq_sfd_num_wake_up_sens_br
  )


  # BPM and POA are factors when imputing
  main_data_impute <- merged_data %>%
    select(src_subject_id, eventname, all_of(vars_abcd)) %>%
    mutate(across(matches("ksads|bpm|poa"), as.factor)) %>% # check factor and numeric
    mutate(across(matches("mctq"), as.numeric))


  id_cols= c("src_subject_id", "eventname")

  main_data_bl <- main_data_impute %>% filter(eventname == "baseline_year_1_arm_1") %>% remove_empty("cols") %>% filter(!if_all(-all_of(id_cols), is.na))
  main_data_1y <- main_data_impute %>% filter(eventname == "1_year_follow_up_y_arm_1") %>% remove_empty("cols") %>% filter(!if_all(-all_of(id_cols), is.na))
  main_data_2y <- main_data_impute %>% filter(eventname == "2_year_follow_up_y_arm_1") %>% remove_empty("cols") %>% filter(!if_all(-all_of(id_cols), is.na))
  main_data_3y <- main_data_impute %>% filter(eventname == "3_year_follow_up_y_arm_1") %>% remove_empty("cols") %>% filter(!if_all(-all_of(id_cols), is.na))
  main_data_4y <- main_data_impute %>% filter(eventname == "4_year_follow_up_y_arm_1") %>% remove_empty("cols") %>% filter(!if_all(-all_of(id_cols), is.na))


  # impute without demographics
  impute_data <- function(data) {
    data_imputed <- missForest(as.data.frame(data %>% select(-all_of(id_cols))))$ximp
    return(data_imputed)
  }

  main_data_bl_imputed <- impute_data(main_data_bl)
  main_data_1y_imputed <- impute_data(main_data_1y)
  main_data_2y_imputed <- impute_data(main_data_2y)
  main_data_3y_imputed <- impute_data(main_data_3y)
  main_data_4y_imputed <- impute_data(main_data_4y)

  main_data_bl_imputed <- main_data_bl_imputed %>% bind_cols(main_data_bl %>% select(all_of(id_cols)))
  main_data_1y_imputed <- main_data_1y_imputed %>% bind_cols(main_data_1y %>% select(all_of(id_cols)))
  main_data_2y_imputed <- main_data_2y_imputed %>% bind_cols(main_data_2y %>% select(all_of(id_cols)))
  main_data_3y_imputed <- main_data_3y_imputed %>% bind_cols(main_data_3y %>% select(all_of(id_cols)))
  main_data_4y_imputed <- main_data_4y_imputed %>% bind_cols(main_data_4y %>% select(all_of(id_cols)))


  main_data_imputed <-
    main_data_bl_imputed %>%
    bind_rows(main_data_1y_imputed) %>%
    bind_rows(main_data_2y_imputed) %>%
    bind_rows(main_data_3y_imputed) %>%
    bind_rows(main_data_4y_imputed)


  merged_data_imputed <- merged_data %>%
    select(-all_of(vars_abcd)) %>%
    left_join(main_data_imputed)

  # Re-create the symptoms criteria based on the imputed variables
  merged_data_imputed <- merged_data_imputed %>%
    mutate(across(matches("bpm|poa"), as.numeric)) %>%
    mutate(
      bpm_18_y_br = case_when(bpm_18_y == 2 ~ 1, bpm_18_y < 2 ~ 0, TRUE ~ NA_real_), # Main
      bpm_18_y_sens_br = case_when(bpm_18_y == 2 ~ 1, TRUE ~ as.numeric(bpm_18_y)), # Sensitivity
      bpm_9_y_br = case_when(bpm_9_y == 2 ~ 1, bpm_9_y < 2 ~ 0, TRUE ~ NA_real_), # Main and sens
      bpm_12_y_sens_br = case_when(bpm_12_y == 2 ~ 1, bpm_12_y < 2 ~ 0, TRUE ~ NA_real_), # Sensitivity
      bpm_4_y_br = case_when(bpm_4_y == 2 ~ 1, bpm_4_y < 2 ~ 0, TRUE ~ NA_real_), # Main and sens

      poa_nihtb_6_sens_br = case_when(poa_nihtb_6_y > 1 ~ 0, TRUE ~ as.numeric(poa_nihtb_6_y)), # Sensitivity
      poa_nihtb_8_sens_br = case_when(poa_nihtb_8_y <= 2 ~ 1, poa_nihtb_8_y == 3 ~ 0, TRUE ~ NA_real_), # Sensitivity
      poa_nihtb_1_sens_br = case_when(poa_nihtb_1_y <= 2 ~ 1, poa_nihtb_1_y == 3 ~ 0, TRUE ~ NA_real_), # Sensitivity
      poa_nihtb_9_sens_br = case_when(poa_nihtb_9_y <= 2 ~ 1, poa_nihtb_9_y == 3 ~ 0, TRUE ~ NA_real_), # Sensitivity

      sleep_latency_hrs = rowMeans(.[, c("mctq_sd_min_to_sleep_calc", "mctq_fd_min_to_sleep_calc")], na.rm = TRUE),
      mctq_sfd_min_to_sleep_br = case_when(sleep_latency_hrs >= 1 ~ 1, sleep_latency_hrs < 1 ~ 0, TRUE ~ NA_real_),
      mctq_sfd_num_wake_up_mean = rowMeans(.[, c("mctq_sd_num_wake_up", "mctq_fd_num_wake_up")], na.rm = TRUE),
      mctq_sfd_num_wake_up_br = case_when(mctq_sfd_num_wake_up_mean >= 3 ~ 1, mctq_sfd_num_wake_up_mean < 3 ~ 0, TRUE ~ NA_real_), # Main
      mctq_sfd_num_wake_up_sens_br = case_when(mctq_sfd_num_wake_up_mean >= 2 ~ 1, mctq_sfd_num_wake_up_mean < 2 ~ 0, TRUE ~ NA_real_)  # Sensitivity
    )


  # saveRDS(merged_data_imputed, "data/merged_data_imputed.rds")
  return(merged_data_imputed)

}


calculate_dep_vars_abcd <- function(data) {

  #--------------------------------------------------------------------------------------------------------------------------------
  #_________________________________________ CREATE DEPRESSION SCORES (IGNORE TP NOT ADMINISTERED) ________________________
  ## Calculate depression score - taking into account only timepoints administered
  ## For timepoints that do not administer the instrument, we ignore the missingness of the items coming from that instrument
  ## (e.g., ksads_1_160_t missingness is ignored in 1y, because no one got that test).

  data <- data %>% mutate(depression_m8 = bpm_4_y_br)
  # Calculate symptoms at each timepoint
  ## Baseline
  data_bl <- data %>% filter(eventname == "baseline_year_1_arm_1")
  #### Main A1-A2-A6-A7
  data_bl <- create_ever_var(data = data_bl, search_term = "_1_[1234]_t_br$", new_col_name = "depression_m1") # A.1.main - no BPM at bl
  data_bl <- create_ever_var(data = data_bl, search_term = "_1_(5|6|179|180)_t_br$", new_col_name = "depression_m2") # A.2.main - no BPM at bl
  data_bl <- create_ever_var(data = data_bl, search_term = "_1_(159|160)_t_br$", new_col_name = "depression_m6") # A.6.main - no BPM at bl
  data_bl <- create_ever_var(data = data_bl, search_term = "_1_(177|178)_t_br$", new_col_name = "depression_m7") # A.7.main - no BPM at bl

  #### Sens A1-A2-A6-A7-A8
  data_bl <- create_ever_var(data = data_bl, search_term = "_1_[1234]_t_br$", new_col_name = "depression_s1") # A.1.sens - no BPM at bl
  data_bl <- create_ever_var(data = data_bl, search_term = "_1_(5|6|179|180)_t_br$", new_col_name = "depression_s2") # A.2.sens - no POA at bl
  data_bl <- create_ever_var(data = data_bl, search_term = "_1_(159|160)_t_br$", new_col_name = "depression_s6") # A.6.sens - no POA at bl
  data_bl <- create_ever_var(data = data_bl, search_term = "_1_(181|182|177|178)_t_br$", new_col_name = "depression_s7") # A.7.sens - no BPM at bl
  data_bl <- create_ever_var(data = data_bl, search_term = "_1_(163|164)_t_br$", new_col_name = "depression_s8") # A.8.sens - no BPM & POA at bl

  data_bl <- create_dep_scores(data = data_bl, main_term = "_m[1267]$", sens_term = "_s[12678]$") # Main Ignore A4, A8(bpm_4_y_br) # Sens Ignore A4

  ## 1Y
  data_1y <- data %>% filter(eventname == "1_year_follow_up_y_arm_1")
  #### Main A1-A7-A8(bpm_4_y_br)
  data_1y <- data_1y %>% mutate(depression_m1 = bpm_18_y_br) # A.1.main - no KSADS at 1y
  data_1y <- data_1y %>% mutate(depression_m7 = bpm_9_y_br)  # A.7.main - no KSADS at 1y

  #### Sens A1-A2-A6-A7-A8
  data_1y <- data_1y %>% mutate(depression_s1 = bpm_18_y_sens_br) # A.1.sens - no KSADS at 1y
  data_1y <- data_1y %>% mutate(depression_s2 = poa_nihtb_6_sens_br)  # A.2.sens - no KSADS at 1y
  data_1y <- data_1y %>% mutate(depression_s6 = poa_nihtb_8_sens_br)  # A.6.sens - no KSADS at 1y
  data_1y <- create_ever_var(data = data_1y, search_term = "bpm_9_y_br|bpm_12_y_sens_br", new_col_name = "depression_s7") # A.7.sens - no KSADS at 1y
  data_1y <- create_ever_var(data = data_1y, search_term = "bpm_4_y_br|nihtb_[19]_sen", new_col_name = "depression_s8") # A.8.sens - no KSADS at 1y

  data_1y <- create_dep_scores(data = data_1y, main_term = "_m[178]$", sens_term = "_s[12678]$") # Main Ignore A2, A4, A6 # Sens Ignore A4

  ## 2Y
  data_2y <- data %>% filter(eventname == "2_year_follow_up_y_arm_1")
  #### Main
  data_2y <- create_ever_var(data = data_2y, search_term = "_1_[1234]_t_br$|bpm_18_y_br", new_col_name = "depression_m1") # A.1.main
  data_2y <- create_ever_var(data = data_2y, search_term = "_1_(5|6|179|180)_t_br$", new_col_name = "depression_m2") # A.2.main
  data_2y <- create_ever_var(data = data_2y, search_term = "(sleep|wake_up)_br$", new_col_name = "depression_m4") # A.4.main
  data_2y <- create_ever_var(data = data_2y, search_term = "_1_(159|160)_t_br$", new_col_name = "depression_m6") # A.6.main
  data_2y <- create_ever_var(data = data_2y, search_term = "_1_(177|178)_t_br$|bpm_9_y_br", new_col_name = "depression_m7") # A.7.main

  #### Sensitivity
  data_2y <- create_ever_var(data = data_2y, search_term = "_1_[1234]_t_br$|bpm_18_y_sens_br", new_col_name = "depression_s1") # A.1.sens
  data_2y <- create_ever_var(data = data_2y, search_term = "_1_(5|6|179|180)_t_br$", new_col_name = "depression_s2") # A.2.sens - No POA at 2Y
  data_2y <- create_ever_var(data = data_2y, search_term = "(sleep|wake_up_sens)_br$", new_col_name = "depression_s4") # A.4.main
  data_2y <- create_ever_var(data = data_2y, search_term = "_1_(159|160)_t_br$", new_col_name = "depression_s6") # A.6.sens - No POA at 2Y
  data_2y <- create_ever_var(data = data_2y, search_term = "_1_(181|182|177|178)_t_br$|bpm_9_y_br|bpm_12_y_sens_br", new_col_name = "depression_s7") # A.7.sens
  data_2y <- create_ever_var(data = data_2y, search_term = "_1_(163|164)_t_br$|bpm_4_y_br", new_col_name = "depression_s8") # A.8.sens - No POA at 2Y

  data_2y <- create_dep_scores(data = data_2y, main_term = "_m[124678]$", sens_term = "_s[124678]$")

  ## 3Y
  data_3y <- data %>% filter(eventname == "3_year_follow_up_y_arm_1")
  ## Main
  data_3y <- data_3y %>% mutate(depression_m1 = bpm_18_y_br) # A.1.main - No KSADS at 3Y
  data_3y <- create_ever_var(data = data_3y, search_term = "(sleep|wake_up)_br$", new_col_name = "depression_m4") # A.4.main - No KSADS at 3Y
  data_3y <- data_3y %>% mutate(depression_m7 = bpm_9_y_br) # A.7.main - No KSADS at 3Y

  ## Sensitivity
  data_3y <- data_3y %>% mutate(depression_s1 = bpm_18_y_sens_br) # A.1.sens - No KSADS at 3Y
  data_3y <- data_3y %>% mutate(depression_s2 = poa_nihtb_6_sens_br) # A.2.sens - No KSADS at 3Y
  data_3y <- create_ever_var(data = data_3y, search_term = "(sleep|wake_up_sens)_br$", new_col_name = "depression_s4") # A.4.main - No KSADS at 3Y
  data_3y <- data_3y %>% mutate(depression_s6 = poa_nihtb_8_sens_br) # A.6.sens - No KSADS at 3Y
  data_3y <- create_ever_var(data = data_3y, search_term = "bpm_9_y_br|bpm_12_y_sens_br", new_col_name = "depression_s7") # A.7.sens - No KSADS at 3Y
  data_3y <- create_ever_var(data = data_3y, search_term = "bpm_4_y_br|nihtb_[19]_sen", new_col_name = "depression_s8") # A.8.sens - No KSADS at 3Y

  data_3y <- create_dep_scores(data = data_3y, main_term = "_m[1478]$", sens_term = "_s[124678]$") # Main Ignore A2, A6

  ## 4Y
  data_4y <- data %>% filter(eventname == "4_year_follow_up_y_arm_1")
  #### Main
  data_4y <- create_ever_var(data = data_4y, search_term = "_1_[1234]_t_br$|bpm_18_y_br", new_col_name = "depression_m1") # A.1.main
  data_4y <- create_ever_var(data = data_4y, search_term = "_1_(5|6|179|180)_t_br$", new_col_name = "depression_m2") # A.2.main
  data_4y <- create_ever_var(data = data_4y, search_term = "(sleep|wake_up)_br$", new_col_name = "depression_m4") # A.4.main
  data_4y <- create_ever_var(data = data_4y, search_term = "_1_(159|160)_t_br$", new_col_name = "depression_m6") # A.6.main
  data_4y <- create_ever_var(data = data_4y, search_term = "_1_(177|178)_t_br$|bpm_9_y_br", new_col_name = "depression_m7") # A.7.main

  #### Sensitivity
  data_4y <- create_ever_var(data = data_4y, search_term = "_1_[1234]_t_br$|bpm_18_y_sens_br", new_col_name = "depression_s1") # A.1.sens
  data_4y <- create_ever_var(data = data_4y, search_term = "_1_(5|6|179|180)_t_br$", new_col_name = "depression_s2") # A.2.sens - No POA at 4y
  data_4y <- create_ever_var(data = data_4y, search_term = "(sleep|wake_up_sens)_br$", new_col_name = "depression_s4") # A.4.main
  data_4y <- create_ever_var(data = data_4y, search_term = "_1_(159|160)_t_br$", new_col_name = "depression_s6") # A.6.sens - No POA at 4y
  data_4y <- create_ever_var(data = data_4y, search_term = "_1_(181|182|177|178)_t_br$|bpm_9_y_br|bpm_12_y_sens_br", new_col_name = "depression_s7") # A.7.sens
  data_4y <- create_ever_var(data = data_4y, search_term = "_1_(163|164)_t_br$|bpm_4_y_br", new_col_name = "depression_s8") # A.8.sens - No POA at 4y

  data_4y <- create_dep_scores(data = data_4y, main_term = "_m[124678]$", sens_term = "_s[124678]$")

  combined_data <- data_bl %>% bind_rows(data_1y) %>% bind_rows(data_2y) %>% bind_rows(data_3y) %>% bind_rows(data_4y)

  return(combined_data)
}
