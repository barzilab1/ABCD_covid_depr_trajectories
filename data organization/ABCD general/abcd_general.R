# library(dplyr)
# library(readr)
# library(stringr)
# source("configurations/path_config.R")

get_abcd_general <- function(){
  abcd_general <- read_csv(file.path(abcd_core_general_path, "abcd_y_lt.csv"),
                      col_types = cols(.default = "n",
                                       "src_subject_id" = "c",
                                       "eventname" = "c",
                                       "interview_date" = "?"))

  ## baseline family ids
  family_ids <- abcd_general %>%
     filter(eventname == "baseline_year_1_arm_1") %>%
     select(matches("src|family"))


  ## longitudinal
  abcd_general <- abcd_general %>%
     select(matches("src|event|interview|site"))  %>%
     mutate(site_id_l_br = sub("site", "", site_id_l),
            interview_date = mdy(interview_date)) %>%
     select(-site_id_l)  %>%
    filter(str_detect(eventname, "year"))

  # Merge baseline with longitudinal
  abcd_general_cb <- left_join(abcd_general, family_ids)

  return(abcd_general_cb)

}
