library(tidyverse)

source("analysis/mplus.R")
source("configurations/path_config.R")

# Only need for main 3 classes in step 5 demo_comparisons and step 6, 8- remove the function
# The list of Pre and Post covid variables
vars <- c(
  "eventname", "interview_date", "interview_age",
  "household_income", "parents_high_edu_text_br",
  # Mental health # mh_p_fhx
  "famhx_ss_fath_prob_dprs_p", "famhx_ss_moth_prob_dprs_p", "famhx_ss_momdad_dprs_p",
  # Parent monitoring # ce_y_pm
  "pmq_y_ss_mean",
  # Exposome
  "exposome_score_1y",
  # Family Environment Scale [Youth] # ce_y_fes
  "fes_y_ss_fc",
  # Prosocial Behavior [Youth] # ce_y_psb
  "psb_y_ss_mean",
  # Wills Problem Solving Scale [Youth] # ce_y_wps
  "wps_ss_sum",
  # Cyberbullying [Youth] # mh_y_cbb
  "cybb_phenx_harm",
  # Peer Experiences Questionnaire [Youth] # mh_y_peq
  "bully_vic", "bully_vic_90_q",
  # ADI # led_l_adi
  "reshist_addr1_adi_wsum",
  # Negative life event # mh_y_le
  "ple_y_ss_total_bad",
  # psychosis, attention-deficit, and anxiety data,
  # and the BPM internalizing, externalizing, and totals
  "ksads_y_depression_diagnosis",
  "ksads_y_anxiety_diagnosis", "ksads_y_bipolar_diagnosis", "ksads_y_sleep_diagnosis",
  "ksads_p_adhd_diagnosis", "ksads_p_conduct_diagnosis",
  "ksads_p_ocd_diagnosis", "ksads_p_ptsd_diagnosis",  "ksads_p_odd_diagnosis", "ksads_p_psychosis_diagnosis",
  "bpm_y_scr_attention_r", "bpm_y_scr_attention_t", "bpm_y_scr_internal_r",  "bpm_y_scr_internal_t",  "bpm_y_scr_external_r",  "bpm_y_scr_external_t",
  "bpm_y_scr_totalprob_r", "bpm_y_scr_totalprob_t"
)


# Get ABCD data
abcd_data_dep_score = read_csv("data/abcd_data_dep_score.csv")

vars_demo <- abcd_data_dep_score %>% select(sex_br, matches("race_(wh|bl|as|ai|nh|mi|ot)|his|MDD_PRS|genetic_afr|born_in_usa")) %>% names()

abcd_data_dep_score <- abcd_data_dep_score %>%
  filter(before_covid == 1 | after_covid == 1) %>%
  mutate(suffix = case_when(
    before_covid == 1 ~ "preCovid",
    after_covid == 1 ~ "postCovid",
    TRUE ~ NA_character_
  ))

# select only variables required for pre/post analysis
abcd_data_dep_score <- abcd_data_dep_score %>% select(all_of(c(vars_demo, vars, "src_subject_id", "suffix", "site_id_l_br", "interview_age", "interview_date")))


abcd_data_dep_score_wide <- abcd_data_dep_score %>%
  pivot_wider(
    names_from = suffix,                # Equivalent to timevar in reshape
    values_from = all_of(c(vars, "site_id_l_br", "interview_age", "interview_date")), # Equivalent to v.names in reshape
    names_sep = "_"                     # Separator for new column names
  )

# Add age at covid beginning march 2020
abcd_data_dep_score_wide <- abcd_data_dep_score_wide %>%
  mutate(age_Mar2020 = (as.numeric(time_length(interval(interview_date_preCovid, "2020-03-01"), unit = "months")) + interview_age_preCovid)/12 ) %>%
  select(-interview_date_preCovid, -interview_age_preCovid, -interview_date_postCovid, -interview_age_postCovid )


# Get class membership
# Read Mplus results
main_name_domain <- "4bins_postC_3_classes_no_covar_random_int_depm05_NOTign_121724"

name_gh5 <- paste0(main_name_domain, ".gh5")
name_dat <- paste0(main_name_domain, ".dat")

path_dat <- file.path(project_path, "mplus_outputs", name_dat)
path_gh5 <- file.path(project_path, "mplus_outputs", name_gh5)

# Read .dat file and process
dat <- read.table(path_dat)
vars_gh5 <- pull(data.frame(mplus.list.variables(path_gh5)))
dat <- setNames(dat, vars_gh5)

# Extract class membership
class_membership <- dat %>%
  select(rid = RID, class = CLASS)

data_wide_4bins <- read_csv("data/data_wide_4bins.csv") %>%
  mutate(rid = row_number())

# Merge datasets
data_wide_4bins_class <- data_wide_4bins %>%
  left_join(class_membership) %>%
  left_join(abcd_data_dep_score_wide) %>%
  select(-c("...1", "rid"))

data_wide_4bins_class <- data_wide_4bins_class %>%
  mutate(class_name = case_when(class == 3 ~ "Resilient",
                                class == 2 ~ "Susceptible",
                                TRUE ~ "Chronic"), # Select classes (for main trajectories)
         late_pubertal = as.factor(as.numeric(puberty_both_sexes > 3)))

# Save merged data to a CSV file
write.csv(data_wide_4bins_class, file.path("outputs", "data_wide_4bins_class.csv"), row.names = F)
