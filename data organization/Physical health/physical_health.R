# library(dplyr)
# library(readr)
# source("configurations/path_config.R")
# source("data organization/data_utility_fun.R")


get_physical_health <- function(files_list){

  datasets_list = list()

  ############################################################
  # Munich Chronotype Questionnaire [Youth]
  ############################################################
  if("ph_y_mctq" %in% files_list){
    MCQ <- read_csv(file.path(abcd_core_physical_health_path, "ph_y_mctq.csv"))

    MCQ <- MCQ %>%
      select(matches("src|event|sleep|get_up|wake_up|btime|sdweek|sd(w|f)_calc$"))

    # sleep duration, latency (time to fall asleep), and inertia (time drowsy after awakening)
    MCQ <- MCQ %>%
      mutate(
        ## Sleep Latency : average of mctq_sd_min_to_sleep_calc, mctq_fd_min_to_sleep_calc (free day and workday) -- these are in hour
        sleep_latency_hrs = rowMeans(.[, c("mctq_sd_min_to_sleep_calc", "mctq_fd_min_to_sleep_calc")], na.rm = TRUE),
        mctq_sfd_min_to_sleep_br = case_when(sleep_latency_hrs >= 1 ~ 1, sleep_latency_hrs < 1 ~ 0, TRUE ~ NA_real_),

        # Average of mctq_sd_num_wake_up, mctq_fd_num_wake_up
        mctq_sfd_num_wake_up_mean = rowMeans(.[, c("mctq_sd_num_wake_up", "mctq_fd_num_wake_up")], na.rm = TRUE),
        mctq_sfd_num_wake_up_br = case_when(mctq_sfd_num_wake_up_mean >= 3 ~ 1, mctq_sfd_num_wake_up_mean < 3 ~ 0, TRUE ~ NA_real_),

        mctq_sfd_num_wake_up_sens_br = case_when(mctq_sfd_num_wake_up_mean >= 2 ~ 1, mctq_sfd_num_wake_up_mean < 2 ~ 0, TRUE ~ NA_real_)  # Sensitivity

      )


    datasets_list[["ph_y_mctq"]] = MCQ
  }


  ############################################################
  # Pubertal Development Scale & Menstrual Cycle Survey History [Youth]
  ############################################################

  if("ph_y_pds" %in% files_list){
    puberty_y <- read_csv(file.path(abcd_core_physical_health_path, "ph_y_pds.csv"))

    puberty_y <- read_csv(file.path(abcd_core_physical_health_path, "ph_y_pds.csv"))

    puberty_y <- puberty_y %>% select(-matches("_(y|nt|nm|dk)$|remote|device"))

    puberty_y$male_y_late_or_post_puberty <- ifelse(puberty_y$pds_y_ss_male_cat_2 >= 4, 1,0)
    puberty_y$female_y_late_or_post_puberty <- ifelse(puberty_y$pds_y_ss_female_category_2 >=4, 1,0)
    puberty_y$late_or_post_puberty_both_sexes <- ifelse(is.na(puberty_y$male_y_late_or_post_puberty), puberty_y$female_y_late_or_post_puberty, puberty_y$male_y_late_or_post_puberty )

    puberty_y$puberty_both_sexes <- ifelse(is.na(puberty_y$pds_y_ss_male_cat_2), puberty_y$pds_y_ss_female_category_2, puberty_y$pds_y_ss_male_cat_2)

    datasets_list[["ph_y_pds"]] = puberty_y
  }


  ############################################################
  ############################################################

  # check that all files were loaded
  if(length(files_list) != length(datasets_list)){
    missing_files = setdiff(files_list, names(datasets_list) )
    print(paste0("The following files weren't loaded: ", paste(missing_files, collapse = " ; ")))
  }

  # combine data
  combined_dataframe <- Reduce(\(x, y) merge(x, y, all = T), datasets_list)

  # write_csv(combined_dataframe, "data/physical_health.csv", na = "")
  # return("data/physical_health.csv")
  return(combined_dataframe)

}
