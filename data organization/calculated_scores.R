# library(dplyr)
# library(readr)
# source("configurations/path_config.R")

get_calculated_scores <- function(calculated_scores, merged_demographics_general_data){

  datasets_list = list()

  ############################################################
  # Allostatic Load
  ############################################################
  if("AL" %in% calculated_scores){
    al_set <- read_csv(allostatic_load_files_path) %>%
      select(src_subject_id = ID,
             al_general = bifactor_general )

    al_set = merge(merged_demographics_general_data, al_set, all.x = T)

    datasets_list[["AL"]] = al_set
  }


  ############################################################
  # Exposome [Tyler]
  ############################################################
  if("exposome" %in% calculated_scores){
    exposome_set <- read_csv(file.path(e_factor_files_path, "ABCD_Exposome_bifactor_scores_16March2021.csv")) %>%
      mutate(ID = paste0("NDAR_", ID)) %>% # data at 1y FU
      select(src_subject_id = ID, exposome_score_1y = Adversity_General_Factor, everything())

    datasets_list[["exposome"]] = exposome_set
  }


  ############################################################
  # genetics
  ############################################################
  if("genetics" %in% calculated_scores){
    genetics_set <- read_csv(abcd_genetics_files_path)

    # genetics_set = merge(merged_demographics_general_data, genetics_set, all.x = T)

    datasets_list[["genetics"]] = genetics_set
  }

  ############################################################
  ############################################################

  # check that all files were loaded
  if(length(calculated_scores) != length(datasets_list)){
    missing_files = setdiff(calculated_scores, names(datasets_list) )
    print(paste0("The following files weren't loaded: ", paste(missing_files, collapse = " ; ")))
  }

  # combine data
  combined_dataframe <- Reduce(\(x, y) merge(x, y, all = T), datasets_list)

  return(combined_dataframe)
}


