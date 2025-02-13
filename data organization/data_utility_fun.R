# library(lubridate)
# library(data.table)
# library(stringr)
# library(missForest)
# library(cdcanthro) # install.packages( 'https://raw.github.com/CDC-DNPAO/CDCAnthro/master/cdcanthro_0.1.2.tar.gz', type='source', repos=NULL )

get_income_to_needs_ratio <- function(df) {

  ########### income to needs ratio
  # SES was estimated using the income-to-needs ratio (INR).
  # The INR was calculated by dividing reported household income by the federal poverty threshold for a given household size.
  # A lower INR ratio indicated higher SES.
  # Income was reported in bins and was adjusted to the median for each income bin.
  # We used the 2017 federal poverty level for the corresponding household sizes
  # The U.S. Census Bureau defines “deep poverty” as living in a household with a total cash income below 50 percent of its poverty threshold.

  df_ = df[,c("src_subject_id", "eventname", "interview_date", "household_income", "demo_roster_v2_br") ]
  df_$year = year(df_$interview_date)

  df_ <- df_ %>%
    mutate(median_income = case_when(
      household_income == 1 ~ median(seq(1, 4999, 1)),
      household_income == 2 ~ median(seq(5000, 11999, 1)),
      household_income == 3 ~ median(seq(12000, 15999, 1)),
      household_income == 4 ~ median(seq(16000, 24999, 1)),
      household_income == 5 ~ median(seq(25000, 34999, 1)),
      household_income == 6 ~ median(seq(35000, 49999, 1)),
      household_income == 7 ~ median(seq(50000, 74999, 1)),
      household_income == 8 ~ median(seq(75000, 99999, 1)),
      household_income == 9 ~ median(seq(100000, 199999, 1)),
      household_income == 10 ~ median(seq(200000, 299999, 1))
      # 200000 is higher than the threshold of a household of 20 people (maximum number of household members in ABCD study is 19) (41320 + 12*4180 = 91480)
      # assume the max of group 10 is 299999 so that median is 249999.5 (higher than 200k
    ))

  df_ <- df_ %>%
    # add poverty threshold according to the number of hh members
    # https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2017-poverty-guidelines
    mutate(pov_threshold = case_when(
      year <= 2017 ~ 12060 + (demo_roster_v2_br -1)*4180,
      year == 2018 ~ 12140 + (demo_roster_v2_br -1)*4320,
      year == 2019 ~ 12490 + (demo_roster_v2_br -1)*4420,
      year == 2020 ~ 12760 + (demo_roster_v2_br -1)*4480,
      year >= 2021 ~ 12880 + (demo_roster_v2_br -1)*4540,
      # year == 2022 ~ 12760 + (demo_roster_v2_br -1)*4480,
    .default = NA
    ))

  df_ <- df_ %>%
    mutate(
      income_to_needs_ratio_br = median_income/pov_threshold,
      fam_under_poverty_line_br = case_when(
        median_income < pov_threshold ~ 1,
        median_income >= pov_threshold ~ 0,
        .default = NA
        ))

  df = merge(df, df_[,c("src_subject_id", "eventname", "income_to_needs_ratio_br", "fam_under_poverty_line_br")])

  return(df)
}


create_ever_var <- function(data, search_term, new_col_name, NA0is0 = F) { # NA0is0 = F for like suicide

  if(length(search_term) > 0){
    search_term = paste0(search_term, collapse = "|")
  }
  matching_cols <- grepl(search_term, colnames(data))

  data_df <- data %>%
    mutate(!!new_col_name := as.integer(apply(data[, matching_cols], 1, \(x) any(x == 1)*1)))

  if(NA0is0) {
    data_df <- data_df %>%
      mutate(!!new_col_name := ifelse(is.na(get(new_col_name)) & apply(data[, matching_cols], 1, \(x) any(x == 0)), 0, get(new_col_name)))
  }

  return(data_df)
}


create_diagnosis_var = function(data, matching_cols, NA0is0 ){
  apply(data[, matching_cols], 1,
        \(x){
          res = any(x == 1)*1
          if(NA0is0 & is.na(res) & any(!is.na(x)) ){
            res = 0
          }
          return(res)
        }
  )
}


# Impute the average for missing timepoint
impute_mean_covid <- function(data = data, search_term) {

  # Check data of search_term to see which timepoint
  empty_cols <- data %>% select(contains(search_term)) %>% select_if(colSums(is.na(.)) == nrow(.)) %>% names()

  if(length(empty_cols) == 0) {data_impute <- data} else {

    if(unique((as.numeric(str_sub(empty_cols, start = -1)) %% 2) == 0)) { # empty timepoint 2-4-6 (have data at tp 1357)

      data_impute <- data %>%
        mutate(
          !!(empty_cols[1]) := rowMeans(data %>% select(matches(paste0(search_term, "____", c(1,3)))), na.rm = T), #____cv
          !!(empty_cols[2]) := rowMeans(data %>% select(matches(paste0(search_term, "____", c(3,5)))), na.rm = T),
          !!(empty_cols[3]) := rowMeans(data %>% select(matches(paste0(search_term, "____", c(5,7)))), na.rm = T)
        )

    }

    if(unique((as.numeric(str_sub(empty_cols, start = -1)) %% 2) == 1)) {  # empty timepoint 1-3-5-7 (have data at tp 246)

      data_impute <- data %>%
        mutate(
          !!(empty_cols[1]) := get(paste0(search_term, "____", 2)), # TP1 get values from TP2
          !!(empty_cols[2]) := rowMeans(data %>% select(matches(paste0(search_term, "____", c(2,4)))), na.rm = T),
          !!(empty_cols[3]) := rowMeans(data %>% select(matches(paste0(search_term, "____", c(4,6)))), na.rm = T),
          !!(empty_cols[4]) := get(paste0(search_term, "____", 6)) # TP7 get values from TP6
        )

    }
  }

  data_impute <- data_impute %>% mutate_all(~ifelse(is.nan(.), NA, .))

  data_impute <- data_impute %>% select(src_subject_id, matches(search_term))

  return(data_impute)
}


create_dep_scores <- function(data, main_term, sens_term) {
  data <- data %>%
    mutate(
      # Sum of available scores
      depression_main = if_else(
        rowSums(!is.na(select(., matches(main_term)))) > 0,
        rowSums(select(., matches(main_term)), na.rm = TRUE),
        NA_real_
      ),
      depression_sens = if_else(
        rowSums(!is.na(select(., matches(sens_term)))) > 0,
        rowSums(select(., matches(sens_term)), na.rm = TRUE),
        NA_real_
      )
    )

  return(data)
}














