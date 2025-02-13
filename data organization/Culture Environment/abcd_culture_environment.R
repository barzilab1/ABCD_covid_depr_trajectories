# library(dplyr)
# library(readr)
# source("configurations/path_config.R")


get_culture_environment <- function(files_list){

  datasets_list = list()

  ############################################################
  # Parental Monitoring [Youth]
  ############################################################
  if("ce_y_pm" %in% files_list){
    parent_mornitor <- read_csv(file.path(abcd_core_culture_environment_path, "ce_y_pm.csv")) %>% select(-matches("(nt|nm|admin)$"))
    parent_mornitor <- parent_mornitor %>% select(-matches("(nt|nm|admin)$"))

    datasets_list[["ce_y_pm"]] = parent_mornitor
  }


  ############################################################
  # Family Environment Scale [Youth]
  ############################################################
  if("ce_y_fes" %in% files_list){
    fam_env <- read_csv(file.path(abcd_core_culture_environment_path, "ce_y_fes.csv"))
    fam_env <- fam_env %>% select(matches("src|event"), fes_y_ss_fc)

    datasets_list[["ce_y_fes"]] = fam_env
  }


  ############################################################
  # Prosocial Behavior [Youth]
  ############################################################
  if("ce_y_psb" %in% files_list){
    prosocial <- read_csv(file.path(abcd_core_culture_environment_path, "ce_y_psb.csv"))
    prosocial <- prosocial %>% select(-matches("(admin|answered|nt|nm)$"))

    datasets_list[["ce_y_psb"]] = prosocial
  }


  ############################################################
  # Wills Problem Solving Scale [Youth]
  ############################################################
  if("ce_y_wps" %in% files_list){
    prob_solving <- read_csv(file.path(abcd_core_culture_environment_path, "ce_y_wps.csv"))
    prob_solving <- prob_solving %>% select(-matches("(admin|n[mt])$"))

    datasets_list[["ce_y_wps"]] = prob_solving
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

  # write_csv(combined_dataframe, "data/culture_environment.csv", na = "")
  # return("data/culture_environment.csv")
  return(combined_dataframe)

}





