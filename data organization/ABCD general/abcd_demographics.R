# library(dplyr)
# library(readr)
# source("configurations/path_config.R")

get_demographics <- function(){
  demographics_set <- read_csv(file.path(abcd_core_general_path, "abcd_p_demo.csv"),
                               col_types = cols(.default = "n", "src_subject_id" = "c", "eventname" = "c"))

  demographics_set[demographics_set == 777 | demographics_set == 999] <- NA

  ###############################
  ########### baseline variables
  ###############################
  demographics_set <- demographics_set %>%

    ########### sex
    # 0 = Male; 1 = Female
    mutate(sex_br = case_when(demo_sex_v2 >= 3 ~ NA, .default = demo_sex_v2),
           sex_br = sex_br - 1,
           sex = case_when(sex_br == 1 ~ "F", sex_br == 0 ~ "M", .default = NA)) %>%

    ########### ethnicity
    mutate(ethnicity_hisp = demo_ethn_v2 %% 2) %>%

    ########### immigration
    # child's country of birth
    mutate(born_in_usa = case_when(demo_origin_v2 == 189 ~ 1, !is.na(demo_origin_v2) ~ 0, .default = NA)) #%>%

  ########### child race
  demographics_set <- demographics_set %>%
    # White
    rename(race_white = demo_race_a_p___10) %>%

    # Black
    rename(race_black = demo_race_a_p___11) %>%

    # Other
    rename(race_other = demo_race_a_p___25)

  # Asian
  demographics_set$race_asian <- Reduce(`|`, demographics_set[, grep("demo_race_a_p___(1[8:9]|2[0-4])", colnames(demographics_set))])*1

  # AIAN: American Indian and Alaska Native
  demographics_set$race_aian <- Reduce(`|`, demographics_set[, grep("demo_race_a_p___1[2-3]", colnames(demographics_set))])*1

  # NHPI: Native Hawaiian and Other Pacific
  demographics_set$race_nhpi <- Reduce(`|`, demographics_set[, grep("demo_race_a_p___1[4-7]", colnames(demographics_set))])*1

  # Mixed
  demographics_set$race_mixed <- ifelse( rowSums(demographics_set[, c("race_white", "race_black", "race_asian", "race_aian", "race_nhpi", "race_other")]) > 1, 1, 0)

  # NHB - NHW - HISP
  demographics_set = demographics_set %>%
    mutate(
      race_ethnicity_5_text_br = case_when(
        race_ethnicity == 1 ~ "NH_White",
        race_ethnicity == 2 ~ "NH_Black",
        race_ethnicity == 3 ~ "Hispanic",
        race_ethnicity == 4 ~ "Asian",
        race_ethnicity == 5 ~ "Other",
        .default = NA ),
      race_ethnicity_3_text_br = case_when(
        race_black == 1 & ethnicity_hisp == 0 ~ "NH_Black",
        race_white == 1 & ethnicity_hisp == 0 ~ "NH_White",
        ethnicity_hisp == 1 ~ "Hispanic",
        .default = NA
      ))

  # 4 groups race x sex
  demographics_set = demographics_set %>%
    mutate(race_sex = case_when(
      !is.na(sex) & !is.na(race_ethnicity_3_text_br) ~ paste0(sex, "__", race_ethnicity_3_text_br),
      .default = NA))





  #########################################
  ########### multiple timepoints variables
  #########################################
  demographics_set <- demographics_set %>%

    ########### gender
    # 0 = Male; 1 = Female; 2 = Trans male; 3 = Trans female; 4 = Gender queer; 5 = Different;
    mutate(gender_p = coalesce(demo_gender_id_v2, demo_gender_id_v2_l),
           gender_p = gender_p - 1) %>%

    ########### number of household members
    mutate(demo_roster_v2_br = coalesce(demo_roster_v2, demo_roster_v2_l),
           # remove outliers
           demo_roster_v2_br = case_when(demo_roster_v2_br <= 0 | demo_roster_v2_br >= 40 ~ NA, .default = demo_roster_v2_br)) %>%

    ########### family income
    mutate(household_income = coalesce(demo_comb_income_v2, demo_comb_income_v2_l)) %>%

    ########### parents married status
    mutate(parents_married_status = coalesce(demo_prnt_marital_v2, demo_prnt_marital_v2_l),
           parents_married = case_when(parents_married_status > 1 ~ 0, .default = parents_married_status),
           separated_or_divorced = case_when(parents_married_status %in% c(3, 4) ~ 1,
                                             parents_married_status < 3 | parents_married_status > 4 ~ 0,
                                             .default = NA),
           living_with_partner_or_married = case_when(parents_married_status %in% c(1, 6) ~ 1,
                                                      parents_married_status > 1 & parents_married_status < 6 ~ 0,
                                                      .default = NA))

  ########### parents education
  demographics_set <- demographics_set %>%
    mutate(demo_prnt_ed_v2_br = coalesce(demo_prnt_ed_v2, demo_prnt_ed_v2_l, demo_prnt_ed_v2_2yr_l),
           demo_prtnr_ed_v2_br = coalesce(demo_prtnr_ed_v2, demo_prtnr_ed_v2_l, demo_prtnr_ed_v2_2yr_l)) %>%

    # 22 = Less than 1 year of college credit/post-secondary education (or less than 10 classes)
    # 23 = One year or more of college credit, no degree
    # 16 = Associate degree: Occupational, Technical, or Vocational T?
    # 15 = Some college
    mutate(across(c(demo_prnt_ed_v2_br, demo_prtnr_ed_v2_br), ~ case_when(.x == 14 ~ 13, TRUE ~ as.numeric(.x)))) %>% # combine 13 and 14
    mutate(across(c(demo_prnt_ed_v2_br, demo_prtnr_ed_v2_br), ~ case_when(.x %in% c(22, 23) ~ 15, TRUE ~ as.numeric(.x)))) %>%  # 15
    mutate(across(c(demo_prnt_ed_v2_br, demo_prtnr_ed_v2_br), ~ case_when(.x == 17 ~ 16, TRUE ~ as.numeric(.x)))) %>%
    mutate(across(c(demo_prnt_ed_v2_br, demo_prtnr_ed_v2_br), ~ case_when(.x > 16 ~ (.x-1), TRUE ~ as.numeric(.x)))) %>% # make consecutive values
    mutate(across(c(demo_prnt_ed_v2_br, demo_prtnr_ed_v2_br), ~ case_when(.x > 13 ~ (.x-1), TRUE ~ as.numeric(.x)))) # make consecutive values

  demographics_set$parents_high_edu = apply(demographics_set[,c("demo_prnt_ed_v2_br", "demo_prtnr_ed_v2_br")], 1,
                                            \(x) if(all(is.na(x))) return(NA) else max(x, na.rm = T))

  demographics_set <- demographics_set %>%
    mutate(parents_high_edu_text_br = case_when(
      parents_high_edu <= 12 ~ "highschool_below",
      parents_high_edu == 13 ~ "highschool_graduate",
      parents_high_edu < 16 ~ "post_highschooler_education",
      parents_high_edu == 16 ~ "bachelor",
      parents_high_edu > 16 ~ "master_above"
    ))

  ########### economic hardship
  var_hardship_long <- demographics_set %>% select(matches("demo_fam_exp[1-7]_v2_l$")) %>% names() # all economic hardship baseline variables
  demographics_set <- Reduce(coalesce_2var, var_hardship_long, demographics_set)

  demographics_set <- demographics_set %>%
    mutate(demo_fam_poverty_br = rowSums(demographics_set %>% select(matches("demo_fam_exp[1-7]_v2_br")), na.rm = T),
           demo_fam_poverty_br = case_when(rowSums(is.na(demographics_set %>% select(matches("demo_fam_exp[1-7]_v2_br")))) == 7 ~ NA,
                                           .default = demo_fam_poverty_br))


  ########### health insurance
  demographics_set <- demographics_set %>%
    mutate(MEDICAID = demo_med_insur_d_p)

  # in 1 year fu, only 227 have data. assumption: there is no big change in MEDICAID over a year
  demographics_set <- demographics_set %>%
    mutate(timepoint = sub("_year.*", "", eventname)) %>%
    group_by(src_subject_id) %>%
    arrange(timepoint) %>%
    mutate(MEDICAID = ifelse(is.na(MEDICAID) & timepoint == 1, lead(MEDICAID), MEDICAID)) %>%
    ungroup() %>%
    select(-timepoint)


  ############################################################
  ############################################################

  demographics_set <- demographics_set %>%
    select(matches("src|event|^sex|^race_|hisp|usa|separated|married$|income$|edu|highschool|bachelor|master_above|poverty|roster_v2_br|acs_raked_propensity_score|MEDICAID"))

  demo_baseline <- demographics_set %>%
    filter(eventname == "baseline_year_1_arm_1") %>%
    select(matches("src|sex|race_|hisp|usa"))

  demographics_set_long <- demographics_set %>%
    select(-matches("sex|race_|hisp|usa"))


  return(list(demo_baseline = demo_baseline,
              demographics_long = demographics_set_long))
}






