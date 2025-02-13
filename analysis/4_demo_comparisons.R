source("analysis/analysis_utility_fun.R")
source("configurations/path_config.R")
library(rcompanion)

data <- read.csv(file.path("outputs/data_wide_4bins_class.csv"))
prop.table(table(data$class_name))*100

# TODO change colnames(data)[grepl("race_", colnames(data))] to: grep("race_",colnames(data), value = T)
# TODO why we need to separate binary and factor variables?
binary_cols = c(colnames(data)[grepl("race_", colnames(data))],
                "ethnicity_hisp","sex_br","resilient","risk","born_in_usa")
factor_cols = c("fam_under_poverty_line_br","household_income","parents_high_edu_text_br")

# Reformat table: set categorical variables to factor form, check numeric types
data = data %>%
  mutate(household_SES = case_when(household_income %in% 1:4 ~ 1,
                                   household_income %in% 5:6 ~ 2,
                                   household_income %in% 7:8 ~ 3,
                                   household_income %in% 9 ~ 4,
                                   household_income %in% 10 ~ 5,
                                   TRUE ~ NA_real_)) %>%
  mutate_at(c(binary_cols,factor_cols,"household_SES"), as.factor) %>% # Turn categorical/ordinal vars to factor
  select(-c(parents_high_edu_text_br_preCovid, parents_high_edu_text_br_postCovid)) # Remove obsolete columns

# Labels for the demographics comparison tables
demo_vars_labels = list(sex_br ~ "Female sex",
                        age_Mar2020 ~ "Age in March 2020",
                        race_white ~ "White",
                        race_black ~ "Black",
                        race_asian ~ "Asian",
                        race_aian ~ "American Indian/Alaska Native",
                        race_nhpi ~ "Native Hawaiian/Other PI",
                        race_other ~ "Other race",
                        race_mixed ~ "Mixed race",
                        ethnicity_hisp ~ "Hispanic",
                        born_in_usa ~ "Born in USA",
                        household_SES ~ "Household income",
                        parents_high_edu_text_br ~ "Parent education",
                        fam_under_poverty_line_br ~ "Below federal poverty line")

# TODO why not reuse the binary_cols and factor_cols?
demo_data <- select(data,matches("^race_|ethnicity_hisp|born_|household_SES|fam_under_|sex_br|class_name|age_Mar2020|parents_high_edu_text_br|class_name"))

# Function which reads the data table and creates a demographic comparison table with p-values
multi_class_comp <- function(data) {
  table_comp <- data %>%
    tbl_summary(by = "class_name", # Comparison by trajectory class
                statistic = list(all_continuous() ~ "{mean} ({sd})",
                                 all_dichotomous() ~ "{n} ({p}%)"),
                type = list(where(is.numeric) ~ "continuous",
                            where(function(x) is.factor(x) && nlevels(x) == 2) ~ "dichotomous"),
                value = list(where(function(x) is.factor(x) && nlevels(x) == 2) ~ "1"),
                missing = "no",
                digits = list(where(is.numeric) ~ 2),
                label = demo_vars_labels)
  return(table_comp)
}

# Table 1: Comparison of resilient and susceptible trajectory classes
demographics_main <- multi_class_comp(data[data$class_name != "Chronic",])
demographics_main  %>%
  add_p(test = age_Mar2020 ~ "t.test",
        test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE)) %>%
  as_gt() %>%
  gt::gtsave(filename="results/manuscript/Table1_resilient_risk_comparison.docx")

# Supplemental Table S7: Comparison of all three classes in the final model
demographics_supp <- multi_class_comp(data)
demographics_supp  %>%
  add_p(test = age_Mar2020 ~ "oneway.test",
        test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE)) %>%
  as_gt() %>%
  gt::gtsave(filename="results/manuscript/TableS7_resilient_risk_comparison.docx")

# Post-hoc comparisons for Supplemental Table S7
# Pairwise chi-squared test comparing by sex
tabs_sex_risk <- xtabs(~ class_name + sex_br,data=data)
pairwiseNominalIndependence(tabs_sex_risk, fisher = FALSE, gtest  = FALSE, chisq  = TRUE, cramer = FALSE)

# Pairwise chi-squared test comparing by Asian race
tabs_asian_risk <- xtabs(~ class_name + race_asian, data=data)
pairwiseNominalIndependence(tabs_asian_risk, fisher = FALSE, gtest  = FALSE, chisq  = TRUE, cramer = FALSE)

# Pairwise chi-squared test comparing by AIAN race
tabs_aian_risk <- xtabs(~ class_name + race_aian, data=data)
pairwiseNominalIndependence(tabs_aian_risk, fisher = FALSE, gtest  = FALSE, chisq  = TRUE, cramer = FALSE)

# Pairwise chi-squared test comparing by Mixed race
tabs_mixed_risk <- xtabs(~ class_name + race_mixed,data=data)
pairwiseNominalIndependence(tabs_mixed_risk, fisher = FALSE, gtest  = FALSE, chisq  = TRUE, cramer = FALSE)

# Pairwise chi-squared test comparing by parent education
tabs_parentedu_risk <- xtabs(~ class_name + parents_high_edu_text_br,data=data)
pairwiseNominalIndependence(tabs_parentedu_risk, fisher = FALSE, gtest  = FALSE, chisq  = TRUE, cramer = FALSE)

# Pairwise chi-squared test comparing by household income
tabs_income_risk <- xtabs(~ class_name + household_SES,data=data)
pairwiseNominalIndependence(tabs_income_risk, fisher = FALSE, gtest  = FALSE, chisq  = TRUE, cramer = FALSE)

# Tukey's Honestly Significant Difference Test by age in March 2020
TukeyHSD(aov(age_Mar2020 ~ class_name,data=data),conf.level = .95)

