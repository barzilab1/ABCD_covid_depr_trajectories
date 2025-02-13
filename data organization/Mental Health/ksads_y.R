# library(dplyr)
# library(readr)
# library(readxl)
# library(stringi)
# library(purrr)
# source("configurations/path_config.R")
# source("data organization/data_utility_fun.R")

get_ksads_y <- function(ksad_tables){


  if(0 == length(ksad_tables) | !any(grepl("youth", names(ksad_tables)))) return()

  datasets_list = list()

  ksads_y <- read_csv(file.path(abcd_core_mental_health_path, "mh_y_ksads_ss.csv"))
  ksads_y[ksads_y == 555] <- NA # 555 to NA
  ksads_y[ksads_y == 888] <- 0 # 888 to 0
  # For symptom items, we coded them separately if they were deliberately not asked (888)
  # because the individual did not answer screening items affirmatively
  # (i.e., due to branching logic)

  ############################################################
  # KSADS - Diagnoses [Youth]
  ############################################################
  if("diagnoses_youth" %in% names(ksad_tables)) {

    # Coalesced pairs (ksad1 & ksad2)
    dictionary_ksads1_ksads2_pairs <- read_excel(file.path(ksads_coalesce_path, "Ksads1_Ksads2_pairs.xlsx"))

    # get items for each diagnosis
    diagnosis_variables = map(ksad_tables$diagnoses_youth,
                              \(x) dictionary_ksads1_ksads2_pairs %>%
                                filter(`Ran tagging` == x) %>%
                                select("KSADS 1 (Youth)", "KSADS 2 (Youth)") %>%
                                unlist() %>% na.omit() %>% as.vector()
                              ) %>% set_names(ksad_tables$diagnoses_youth)

    # Create the ksads_y_diagnosis data
    ksads_y_diagnosis <- ksads_y %>% select(matches("src|event"), all_of(unlist(diagnosis_variables, use.names = F)))

    # Combine different variables for each diagnosis
    for(diagnosis in ksad_tables$diagnoses_youth){
      ksads_y_diagnosis[[paste0("ksads_y_", tolower(diagnosis), "_diagnosis")]] <- create_diagnosis_var(ksads_y_diagnosis, diagnosis_variables[[diagnosis]], NA0is0 = T )
    }

    datasets_list[["diagnoses_youth"]] = ksads_y_diagnosis
  }


  ############################################################
  # KSADS - Symptoms [Youth]
  ############################################################
  if("symptoms_youth" %in% names(ksad_tables)){

    # Only have data at baseline, 2y and 4y
    ksads_y_symptoms <- ksads_y %>% filter(!eventname %in% c("1_year_follow_up_y_arm_1", "3_year_follow_up_y_arm_1"))


    # Check and merge longitudinal data
    ksads_y_symptoms <- ksads_y_symptoms %>%
      mutate(
        ksads_1_1_t_br = coalesce(ksads_1_1_t, ksads2_1_1_t), # ksads2_1_1_t
        ksads_1_2_t_br = coalesce(ksads_1_2_t, ksads2_1_2_t), # ksads2_1_2_t
        ksads_1_3_t_br = coalesce(ksads_1_3_t, ksads2_1_3_t), # ksads2_1_3_t
        ksads_1_4_t_br = coalesce(ksads_1_4_t, ksads2_1_4_t), # ksads2_1_4_t
        ksads_1_5_t_br = coalesce(ksads_1_5_t, ksads2_1_5_t), # ksads2_1_5_t
        ksads_1_6_t_br = coalesce(ksads_1_6_t, ksads2_1_6_t), # ksads2_1_6_t
        ksads_1_179_t_br = coalesce(ksads_1_179_t, ksads2_1_170_t), # ksads2_1_170_t
        ksads_1_180_t_br = coalesce(ksads_1_180_t, ksads2_1_171_t), # ksads2_1_171_t
        ksads_1_159_t_br = coalesce(ksads_1_159_t, ksads2_1_150_t), # ksads2_1_150_t
        ksads_1_160_t_br = coalesce(ksads_1_160_t, ksads2_1_151_t), # ksads2_1_151_t
        ksads_1_177_t_br = coalesce(ksads_1_177_t, ksads2_1_168_t), # ksads2_1_168_t
        ksads_1_178_t_br = coalesce(ksads_1_178_t, ksads2_1_169_t), # ksads2_1_169_t
        ksads_1_181_t_br = coalesce(ksads_1_181_t, ksads2_1_172_t), # ksads2_1_172_t
        ksads_1_182_t_br = coalesce(ksads_1_182_t, ksads2_1_173_t), # ksads2_1_173_t
        ksads_1_163_t_br = coalesce(ksads_1_163_t, ksads2_1_154_t), # ksads2_1_154_t
        ksads_1_164_t_br = coalesce(ksads_1_164_t, ksads2_1_155_t), # ksads2_1_155_t
      )

    ksads_y_symptoms <- ksads_y_symptoms %>%
      select(matches("src|event|_br$"))


    datasets_list[["symptoms_youth"]] = ksads_y_symptoms
  }


  ############################################################
  ############################################################

  # check that all files were loaded
  # if(length(ksad_tables) != length(datasets_list)){
  #   missing_files = setdiff(ksad_tables, names(datasets_list) )
  #   print(paste0("The following files weren't loaded: ", paste(missing_files, collapse = " ; ")))
  # }

  # combine data
  combined_dataframe <- Reduce(\(x, y) merge(x, y, all = T), datasets_list)

  return(combined_dataframe)
}


