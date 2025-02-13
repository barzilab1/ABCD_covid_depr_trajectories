# library(tidyr)
# library(dplyr)
# library(readr)
# library(purrr)
# source("configurations/path_config.R")
# source("data organization/data_utility_fun.R")


get_covid_19 <- function(files_list){

  datasets_list = list()

  ############################################################
  # COVID-19 - Questionnaire [Youth] # 2020-05-16 to 2021-07-07
  ############################################################
  if("cvd_y_qtn" %in% files_list){
    covid_y <- read_csv(file.path(abcd_sub_covid19_path, "cvd_y_qtn.csv"),
                        col_types = cols(.default = "?",
                                         "src_subject_id" = "c",
                                         "eventname" = "c",
                                         "su_restrictions_cv" = "n"))

    covid_y <- covid_y %>% select(matches("src|event|_(sad|unhappy|fun|sleep|wake_up_cv|wrong)|t_angry|energetic|interested|attentive|concentrate|complete_date"), felt_cv)
    covid_y[covid_y == 777 | covid_y == 999] <- NA
    covid_y <- covid_y %>% mutate(timepoint = substr(eventname, 11, 11))

    # energetic_y_cv, attentive_y_cv, concentrate_y_cv, interested_y_cv: values 1-3-5 - no need to recode to 1-2-3 as only use 1 as cutoff

    covid_y_wide <-
      tidyr::pivot_wider(covid_y %>% select(-eventname),
                         names_from = timepoint,
                         values_from = attentive_y_cv:felt_cv,
                         names_sep = "____")


    var_list <- c("felt_sad_cv", # cv1357
                  "felt_always_sad", # cv1357
                  "felt_unhappy_cv", # cv1357
                  "felt_angry_cv", # cv1357
                  "felt_no_fun_cv", # cv1357
                  "interested_y_cv", # cv246
                  "mctq_fd_min_to_sleep_cv", # all TP
                  "mctq_fd_num_wake_up_cv", # all TP
                  "energetic_y_cv", # cv246
                  "felt_cv", # cv1357
                  "felt_life_went_wrong_cv", # cv1357
                  "attentive_y_cv", # cv246
                  "concentrate_y_cv") # cv246


    # calculate the scores as averages of the 2 timepoints around the timepoint of interest, while ignoring the NAs.
    # T1 = T2, T3 = mean(T2, T4), where if either is NA, then it's just the value of the other, NA if both are NA.

    datasets <- map(var_list, ~ impute_mean_covid(data = covid_y_wide, search_term = .x))

    data_average <- Reduce(\(x, y) merge(x, y, all = T), datasets) # Not include yabcdcovid19_complete_date____


    # Convert data_average to long # We have more data as in original data, there are missing timepoints - here full 7tp for each participant
    covid_y_averaged <- pivot_longer(data_average, !src_subject_id, names_to = c(".value", "timepoint"), names_pattern = "(.*)____(.*)")

    covid_y_averaged <- covid_y_averaged %>%
      right_join(
        covid_y %>% select(matches("src|timepoint|event"), interview_date = yabcdcovid19_complete_date)
      )

    datasets_list[["cvd_y_qtn"]] = covid_y_averaged
  }


  if(length(files_list) != length(datasets_list)){
    missing_files = setdiff(files_list, names(datasets_list) )
    print(paste0("The following files weren't loaded: ", paste(missing_files, collapse = " ; ")))
  }

  # combine data
  combined_dataframe <- Reduce(\(x, y) merge(x, y, all = T), datasets_list)

  return(combined_dataframe)

}
























