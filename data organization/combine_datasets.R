# library(dplyr)
# library(tidyr)

# Create during covid: combined data & 4 bins
create_covid_4_bins <- function(abcd_data, covid_data){

  ## 1 ABCD DATA DURING COVID TIMEFRAME
  abcd_data_during_covid <- abcd_data %>%
    filter(interview_date >= "2020-03-01" & interview_date < "2021-08-01")

  ## 2 CREATE 4 BINS DURING COVID
  # Create bins for the during covid data for both covid and merged_data:
  # March 1, 2020 - August 1, 2020
  # August 1, 2020 - December 1, 2020
  # December 1, 2020 - May 1, 2021
  # May 1, 2021 - July 1, 2021

  merged_covid_abcd_4bins <-
    covid_data %>%
    select(matches("src|timepoint|event|interview|age|depression")) %>%
    bind_rows(
      abcd_data_during_covid %>%
        select(matches("src|timepoint|event|interview_d|depression_[ms]"), age)
    ) %>%
    mutate(bins = case_when(
      interview_date >= "2020-03-01" & interview_date < "2020-08-01" ~ 1,
      interview_date >= "2020-08-01" & interview_date < "2020-12-01" ~ 2,
      interview_date >= "2020-12-01" & interview_date < "2021-05-01" ~ 3,
      interview_date >= "2021-05-01" & interview_date < "2021-08-01" ~ 4,
      TRUE ~ NA_real_
    ))

  # 3 Create wide data for lgmm
  merged_covid_abcd_4bins_mean_wide <-
    # add means of scores within each bins for each # For during covid data (including main ABCD and Covid) - take average of these two scores
    merged_covid_abcd_4bins %>%
    group_by(src_subject_id, bins) %>%
    summarise(depression_main = mean(depression_main, na.rm = T), # depression_main_mean is mean of covid and abcd scores in the same bin
              depression_sens = mean(depression_sens, na.rm = T),
              age_mean_bins = mean(age, na.rm = T)) %>%
    pivot_wider(names_from = bins, values_from = depression_main:age_mean_bins, names_sep = "__") %>%
    select(-ends_with("NA")) %>%
    rename(age_4th_bin = "age_mean_bins__4") %>%
    select(-matches("age_mean"))

  return(list(wide_data = merged_covid_abcd_4bins_mean_wide, long_data = merged_covid_abcd_4bins))
}


create_whole_data <- function(abcd_data_dep_score, covid_data_dep_score, imputed = F){

  merged_covid_abcd_4bins = create_covid_4_bins(abcd_data_dep_score, covid_data_dep_score)

  # 1. wide data
  vars_demo <- abcd_data_dep_score %>% select(age, sex_br, matches("race_(wh|bl|as|ai|nh|mi|ot)|edu"), household_income, fam_under_poverty_line_br, puberty_both_sexes, late_or_post_puberty_both_sexes) %>% names()

  # create pre & post data wide
  pre_covid_data_short_wide <- abcd_data_dep_score %>%
    filter(before_covid == 1) %>%
    # use demographics, family and site ids from this data
    select(matches("src|depression_(main|sens)"), all_of(vars_demo),
           rel_family_id, site_id_l_br) %>%
    rename_with(~ paste0(., "__0"), .cols = matches("depression_(main|sens)")) %>%
    rename(age_pre_covid = "age") # 11867 # TP 0

  post_covid_data_short_wide <- abcd_data_dep_score %>%
    filter( after_covid == 1) %>%
    select(matches("src|depression_(main|sens)")) %>%
    rename_with(~ paste0(., "__5"), .cols = matches("depression_(main|sens)")) # 3594 # TP 5

  # combine all data (wide format): data will include only kids that have a covid and post-covid visit
  data_wide_4bins <- merged_covid_abcd_4bins$wide_data %>%
    left_join(pre_covid_data_short_wide) %>% # demographics, site, family, puberty, income, education
    left_join(post_covid_data_short_wide)

  data_wide_4bins <- data_wide_4bins %>% filter(src_subject_id %in% post_covid_data_short_wide$src_subject_id)


  # 2. long data
  abcd_data_long_bins <- abcd_data_dep_score %>%
    filter(before_covid == 1 | after_covid == 1) %>%
    mutate(bins = case_when(
      before_covid == 1 ~ 0,
      after_covid == 1 ~ 5,
      TRUE ~ NA_real_
    )) %>% select(src_subject_id, eventname, interview_date, bins, matches("^depression"))

  # combine all data (long format): data will include only kids that have a covid and post-covid visit
  data_long_4bins <- merged_covid_abcd_4bins$long_data %>% select(src_subject_id, eventname, interview_date, bins, matches("^depression")) %>%
    bind_rows(abcd_data_long_bins) %>% filter(src_subject_id %in% data_wide_4bins$src_subject_id)


  if(imputed){
    write.csv(data_wide_4bins, "data/data_wide_4bins_imputed.csv")
    write.csv(data_long_4bins, "data/data_long_4bins_imputed.csv")
    write.csv(abcd_data_dep_score, "data/abcd_data_dep_score_imputed.csv", row.names = F)
  }else{
    write.csv(data_wide_4bins, "data/data_wide_4bins.csv")
    write.csv(data_long_4bins, "data/data_long_4bins.csv")
    write.csv(abcd_data_dep_score, "data/abcd_data_dep_score.csv", row.names = F)
  }

  return(data_wide_4bins)

}


statify_data <- function(abcd_data, data_wide){

  # 1. sex
  dat_M <- data_wide %>% filter(sex_br == 0)
  dat_F <- data_wide %>% filter(sex_br == 1)

  write.csv(dat_M, "data/b4_NOTign_M.csv")
  write.csv(dat_F, "data/b4_NOTign_F.csv")

  ## 2. before COVID: post-pubertal
  dat_Post_pubertal_pre_COVID <- data_wide %>% filter(late_or_post_puberty_both_sexes == 1)

  # 3. After Covid: pre-pubertal
  pubertal_post_Covid_dat <- abcd_data %>%
    filter(after_covid == 1) %>%
    select(src_subject_id, puberty_both_sexes_postCovid = late_or_post_puberty_both_sexes)

  dat_Pre_pubertal_post_COVID <- data_wide %>%
    right_join(pubertal_post_Covid_dat %>% filter(puberty_both_sexes_postCovid == 0))

  write.csv(dat_Post_pubertal_pre_COVID, "data/b4_NOTign_PoP_PrC.csv")
  write.csv(dat_Pre_pubertal_post_COVID, "data/b4_NOTign_PrP_PoC.csv")

 return()

}


