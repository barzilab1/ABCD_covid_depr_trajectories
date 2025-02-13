# library(dplyr)
# library(readr)
# library(readxl)
# library(stringi)
# library(purrr)
# source("configurations/path_config.R")
# source("data organization/data_utility_fun.R")

get_ksads_p <- function(ksad_tables){


  if(0 == length(ksad_tables) | !any(grepl("parent", names(ksad_tables)))) return()

  datasets_list = list()

  ksads_p <- read_csv(file.path(abcd_core_mental_health_path, "mh_p_ksads_ss.csv"))
  ksads_p[ksads_p == 555] <- NA # 555 to NA
  ksads_p[ksads_p == 888] <- 0 # 888 to 0
  # For symptom items, we coded them separately if they were deliberately not asked (888)
  # because the individual did not answer screening items affirmatively
  # (i.e., due to branching logic)

  ############################################################
  # KSADS - Diagnoses [Parent]
  ############################################################
  if("diagnoses_parent" %in% names(ksad_tables)) {

    # Coalesced pairs (ksad1 & ksad2)
    dictionary_ksads1_ksads2_pairs <- read_excel(file.path(ksads_coalesce_path, "Ksads1_Ksads2_pairs.xlsx"))

    # get items for each diagnosis
    diagnosis_variables = map(ksad_tables$diagnoses_parent,
                              \(x) dictionary_ksads1_ksads2_pairs %>%
                                filter(`Ran tagging` == x) %>%
                                select("KSADS 1 (Parent)", "KSADS 2 (Parent)") %>%
                                unlist() %>% na.omit() %>% as.vector()
    ) %>% set_names(ksad_tables$diagnoses_parent)

    # Create the ksads_p_diagnosis data
    ksads_p_diagnosis <- ksads_p %>% select(matches("src|event"), all_of(unlist(diagnosis_variables, use.names = F)))

    # Combine different variables for each diagnosis
    for(diagnosis in ksad_tables$diagnoses_parent){
      ksads_p_diagnosis[[paste0("ksads_p_", tolower(diagnosis), "_diagnosis")]] <- create_diagnosis_var(ksads_p_diagnosis, diagnosis_variables[[diagnosis]], NA0is0 = T )
    }

    datasets_list[["diagnoses_parent"]] = ksads_p_diagnosis
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


