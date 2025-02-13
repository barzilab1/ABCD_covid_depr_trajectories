# library(dplyr)
# library(readr)
# library(tidyr)
# library(missForest)
# library(lubridate)
# library(janitor)

# source("data organization/data_utility_fun.R")


calculate_covid_age <- function(covid_19_data, merged_data){

  covid_19_data <- covid_19_data %>% mutate(timepoint = as.numeric(timepoint))

  ## impute missing interview_date or age for covid data
  mean_int_date_covid <- covid_19_data %>%
    group_by(timepoint) %>%
    summarise(mean_int_date = mean(interview_date, na.rm = T)) # interview_date is yabcdcovid19_complete_date

  covid_19_data <- covid_19_data %>%
    left_join(mean_int_date_covid) %>%
    mutate(interview_date = coalesce(interview_date, mean_int_date)) %>%
    select(-mean_int_date)


  tp_before_covid <- merged_data %>%
    filter(before_covid == 1) %>%
    mutate(age_before_covid = interview_age, date_before_covid = interview_date) %>%
    select(src_subject_id, age_before_covid, date_before_covid)


  # Calculate age of covid data
  covid_19_data <- covid_19_data %>%
    # calculate age at completion
    left_join(tp_before_covid) %>%
    mutate(interview_age = as.numeric(time_length(interval(date_before_covid, interview_date), unit = "months")) + age_before_covid,
           age = interview_age / 12) %>%
    select(-age_before_covid, -date_before_covid, -interview_age)


  return(covid_19_data)

}


impute_covid <- function(data) {

  data_impute <- data %>% select(-matches("src|timepoint|event|date|age"))

  imputed_data <- missForest(as.data.frame(data_impute))$ximp

  data_final <- data %>% select(matches("src|timepoint|event|date|age")) %>% bind_cols(imputed_data)

  return(data_final)

}


calculate_dep_vars_covid <- function(data) {

  # Create depression scores
  ## High Specificity Low Sensitivity- Criteria for Main Analysis

  data <- data %>%
    mutate(
      #--- A.1.- Depressed mood	felt_sad_cv	Answering 4-5 	felt sad often or almost always
      #--- A.1.- Depressed mood	felt_always_sad	Answering 4-5	couldn’t stop feeling sad often or almost always
      #--- A.1.- Depressed mood	felt_unhappy_cv	Answering 5	felt unhappy almost always
      #--- A.1.- Depressed mood	felt_unhappy_cv	Answering 4-5	felt unhappy often or almost always # Sensitivity
      #--- A.1.- Depressed mood	felt_angry_cv	Answering 5	felt angry or frustrated all almost the time  # Sensitivity
      felt_sad_cv_br = if_else(felt_sad_cv >= 4, 1, 0, missing = NA_real_),
      felt_always_sad_cv_br = if_else(felt_always_sad >= 4, 1, 0, missing = NA_real_),
      felt_unhappy_cv_br = if_else(felt_unhappy_cv == 5, 1, 0, missing = NA_real_),
      felt_unhappy_cv_sens_br = if_else(felt_unhappy_cv >= 4, 1, 0, missing = NA_real_), # Sensitivity
      felt_angry_cv_sens_br = if_else(felt_angry_cv == 5, 1, 0, missing = NA_real_), # Sensitivity

      ### A.2.- Anhedonia	felt_no_fun_cv	Answering 4-5	Hard to have fun often or almost always
      #--- A.2.- Anhedonia	interested_y_cv	Answering 1	answeing "Not true" to "felt interested"  # Sensitivity
      felt_no_fun_cv_br = if_else(felt_no_fun_cv >= 4, 1, 0, missing = NA_real_),
      interested_y_cv_sens_br = if_else(interested_y_cv == 1, 1, 0, missing = NA_real_),  # Sensitivity
      ### A.4. - If you meet either conditions you get a 1
      #--- A.4. - Sleep problem	mctq_fd_min_to_sleep_cv	Answering 17 or above	takes at least an hour to fall asleep
      #--- A.4. - Sleep problem	mctq_fd_num_wake_up_cv	Answering 3 or above	waking up at least three times after falling asleep
      #--- A.4. - Sleep problem	mctq_fd_num_wake_up_cv	Answering 2 or above	waking up at least twice after falling asleep  # Sensitivity
      mctq_fd_min_to_sleep_cv_br = if_else(mctq_fd_min_to_sleep_cv >= 17, 1, 0, missing = NA_real_),
      mctq_fd_num_wake_up_cv_br = if_else(mctq_fd_num_wake_up_cv >= 3, 1, 0, missing = NA_real_),
      mctq_fd_num_wake_up_cv_sens_br = if_else(mctq_fd_num_wake_up_cv >= 2, 1, 0, missing = NA_real_),  # Sensitivity

      ### A.6.- Low energy	energetic_y_cv	Answering 1	answeing "Not true" for "feeling energetic"
      energetic_y_cv_br = if_else(energetic_y_cv  == 1, 1, 0, missing = NA_real_),

      ### A.7.- Wothlessness, guilt	felt_cv	Answering 4-5	I couldn’t do anything right often or almost always
      #--- A.7.- Wothlessness, guilt	felt_life_went_wrong_cv	Answering 4-5	I felt everything in my life went wrong often or almost always  # Sensitivity
      felt_cv_br = if_else(felt_cv >= 4, 1, 0, missing = NA_real_),
      felt_life_went_wrong_cv_sens_br = if_else(felt_life_went_wrong_cv >= 4, 1, 0, missing = NA_real_),

      ### A.8. - If you meet either conditions you get a 1
      #--- A.8.- Problems in concentration, indecisiveness	attentive_y_cv	Answering 1	"Not true"  for feeling attentive
      #--- A.8.- Problems in concentration, indecisiveness	concentrate_y_cv	Answering 1	"Not true"  for feeling able to concentrate
      attentive_y_cv_br = if_else(attentive_y_cv == 1, 1, 0, missing = NA_real_),
      concentrate_y_cv_br = if_else(concentrate_y_cv == 1, 1, 0, missing = NA_real_)
    )

  data <- create_ever_var(data = data, search_term = "(sad|unhappy)_cv_br", new_col_name = "depression_m1") # A.1.main - If you meet either conditions you get a 1
  data <- data %>% mutate(depression_m2 = felt_no_fun_cv_br)
  data <- create_ever_var(data = data, search_term = "(sleep|up)_cv_br", new_col_name = "depression_m4") # A.4.main - If you meet either conditions you get a 1
  data <- data %>% mutate(depression_m6 = energetic_y_cv_br)
  data <- data %>% mutate(depression_m7 = felt_cv_br)
  data <- create_ever_var(data = data, search_term = "(attentive|concentrate)_y_cv_br", new_col_name = "depression_m8") # A.8.main - If you meet either conditions you get a 1

  data <- create_ever_var(data = data, search_term = "sad_cv_br|(unhappy|angry)_cv_sens", new_col_name = "depression_s1") # A.1.sens - If you meet either conditions you get a 1
  data <- create_ever_var(data = data, search_term = "(fun|inter).*br", new_col_name = "depression_s2") # A.2.sens - If you meet either conditions you get a 1
  data <- create_ever_var(data = data, search_term = "sleep_cv_br|up_cv_s", new_col_name = "depression_s4") # A.4.sens - If you meet either conditions you get a 1
  data <- data %>% mutate(depression_s6 = energetic_y_cv_br)
  data <- create_ever_var(data = data, search_term = "felt_(c|l).*br", new_col_name = "depression_s7") # A.7.sens - If you meet either conditions you get a 1
  data <- data %>% mutate(depression_s8 = depression_m8)


  data = create_dep_scores(data, "m[124678]$", "s[124678]$")

  return(data)

}
