# library(dplyr)
# library(readr)
# source("configurations/path_config.R")


get_mental_health <- function(files_list){

  datasets_list = list()

  ############################################################
  # Brief Problem Monitor [Youth]
  ############################################################
  if("mh_y_bpm" %in% files_list){
    bpm <- read_csv(file.path(abcd_core_mental_health_path, "mh_y_bpm.csv"))
    bpm[bpm == "777" | bpm == "999"] = NA
    bpm <- bpm %>% select(-matches("_(n(m|t)|admin)$"))

    bpm <- bpm %>% mutate(
      bpm_18_y_br = case_when(bpm_18_y == 2 ~ 1, bpm_18_y < 2 ~ 0, TRUE ~ NA_real_), # Main
      bpm_18_y_sens_br = case_when(bpm_18_y == 2 ~ 1, TRUE ~ as.numeric(bpm_18_y)), # Sensitivity
      bpm_9_y_br = case_when(bpm_9_y == 2 ~ 1, bpm_9_y < 2 ~ 0, TRUE ~ NA_real_), # Main and sens
      bpm_12_y_sens_br = case_when(bpm_12_y == 2 ~ 1, bpm_12_y < 2 ~ 0, TRUE ~ NA_real_), # Sensitivity
      bpm_4_y_br = case_when(bpm_4_y == 2 ~ 1, bpm_4_y < 2 ~ 0, TRUE ~ NA_real_) # Main and sens
      )

    datasets_list[["mh_y_bpm"]] = bpm
  }


  ############################################################
  # Cyberbullying [Youth]
  ############################################################
  if("mh_y_cbb" %in% files_list){
    cyber_bully <- read_csv(file.path(abcd_core_mental_health_path, "mh_y_cbb.csv"))
    cyber_bully[cyber_bully == 777 | cyber_bully == 999] = NA
    cyber_bully <- cyber_bully %>% select(-matches("(often|admin|device)$|power|12mo"))

    datasets_list[["mh_y_cbb"]] = cyber_bully
  }


  ########################################################################
  # Peer Experiences Questionnaire [Youth]
  ########################################################################

  if("mh_y_peq" %in% files_list){
    peer_exp_y <- read_csv(file.path(abcd_core_mental_health_path, "mh_y_peq.csv"))

    peer_exp_y <- peer_exp_y %>%
      mutate(
        bully_vic = peq_ss_relational_victim + peq_ss_reputation_victim + peq_ss_overt_victim,

        # binary
        vic_90_q = quantile(bully_vic, probs = 0.9, na.rm = T),
        bully_vic_90_q = case_when(bully_vic > vic_90_q ~ 1, bully_vic <= vic_90_q ~ 0, TRUE ~ NA_real_),

      )

    datasets_list[["mh_y_peq"]] = peer_exp_y
  }

  ########################################################################
  # Family History [Parent] # at baseline only
  ########################################################################
  if("mh_p_fhx" %in% files_list){
    fhx <- read_csv(file.path(abcd_core_mental_health_path, "mh_p_fhx.csv"),
                    col_types = cols(.default = "n", "src_subject_id" = "c", "eventname" = "c"))

    fhx <- fhx %>% select(matches("src|(momdad|fath|moth)(_prob)?_dprs_p"))
    fhx[fhx == 999] = NA

    datasets_list[["mh_p_fhx"]] = fhx
  }

  ############################################################
  # NIH Toolbox - Positive Affect Items [Youth]
  ############################################################
  if("mh_y_poa" %in% files_list){
    mh_y_poa <- read_csv(file.path(abcd_core_mental_health_path, "mh_y_poa.csv"))
    mh_y_poa <- mh_y_poa %>% select(-matches("(admin|nt|nm)$"))
    mh_y_poa[mh_y_poa == 999 | mh_y_poa == 777] <- NA

    mh_y_poa <- mh_y_poa %>% mutate(
      poa_nihtb_6_sens_br = case_when(poa_nihtb_6_y > 1 ~ 0, TRUE ~ as.numeric(poa_nihtb_6_y)), # Sensitivity
      poa_nihtb_8_sens_br = case_when(poa_nihtb_8_y <= 2 ~ 1, poa_nihtb_8_y == 3 ~ 0, TRUE ~ NA_real_), # Sensitivity
      poa_nihtb_1_sens_br = case_when(poa_nihtb_1_y <= 2 ~ 1, poa_nihtb_1_y == 3 ~ 0, TRUE ~ NA_real_), # Sensitivity
      poa_nihtb_9_sens_br = case_when(poa_nihtb_9_y <= 2 ~ 1, poa_nihtb_9_y == 3 ~ 0, TRUE ~ NA_real_), # Sensitivity
    )

    datasets_list[["mh_y_poa"]] = mh_y_poa
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

  return(combined_dataframe)


}






