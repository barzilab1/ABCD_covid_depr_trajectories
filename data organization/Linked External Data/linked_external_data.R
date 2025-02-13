# library(dplyr)
# library(readr)
# source("configurations/path_config.R")


get_linked_external <- function(files_list) {

  datasets_list = list()


  ############################################################
  # Area Deprivation Index (ADI) [Linked Dataset]
  ############################################################

  if("led_l_adi" %in% files_list) {
    led_l_adi <- read_csv(file.path(abcd_core_led_path, "led_l_adi.csv"))
    led_l_adi <- led_l_adi %>% select(matches("src|reshist_addr1_adi_wsum"))

    datasets_list[["led_l_adi"]] = led_l_adi

  }


  ############################################################
  # Child Opportunity Index (COI) [Linked Dataset]
  ############################################################
  if("led_l_coi" %in% files_list) {
    coi <- read_csv(file.path(abcd_core_led_path, "led_l_coi.csv")) %>% select(matches("src|addr1_.*_met$"))

    datasets_list[["led_l_coi"]] = coi

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




