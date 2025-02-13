# library(tidyverse)
# library(janitor)
# source("data organization/data_utility_fun.R")

# some kids are missing demographics from 2 year fu onwards. copy the demographics base data to all time points
merge_demographics_general <- function(demographics_data, abcd_general_data){

  data = merge(abcd_general_data, demographics_data$demo_baseline)
  data = merge(data, demographics_data$demographics_long, all.x = T)
  data = get_income_to_needs_ratio(data)

  data$age  = data$interview_age/12
  return(data)

}

merge_all_data <- function(..., tp_include = NULL){

  datasets = list(...)

  combined_datasets <- Reduce(\(x, y) { if(nrow(y) != 0) merge(x, y, all = T) else x }, datasets)

  if(!is.null(tp_include)){
    combined_datasets <- combined_datasets %>% filter(grepl(paste0(tp_include, collapse = "|"), eventname))
  }
  combined_datasets = remove_empty(combined_datasets, "cols")

  if("bmi" %in% colnames(combined_datasets)) {
    combined_datasets = get_BMI_percentiles(combined_datasets)
  }

  write_csv(combined_datasets, file = "data/data_long.csv", na = "")
  return(combined_datasets)

}


