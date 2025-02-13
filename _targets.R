# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
# remotes::install_github("dxf1david/cdcanthro")
tar_option_set(
  # packages that your targets need to run
  packages = c("tidyverse", "stringi","arrow", "missForest",
               "janitor","cdcanthro", "data.table"),
  format = "feather" # default format, unless specifically specified in targets
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  # , controller = crew::crew_controller_local(workers = 6)
  #

)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("configurations")
tar_source("data organization")


# Replace the target list below with your own:
list(

  #### MAIN STUDY ####
  # load main study data
  tar_target(demographics_data, get_demographics(), format = "rds"),
  tar_target(abcd_general_data, get_abcd_general()),
  tar_target(merged_demographics_general_data, merge_demographics_general(demographics_data, abcd_general_data)),

  tar_target(culture_environment_data, get_culture_environment(culture_environment_tables)),
  tar_target(linked_external_data, get_linked_external(linked_external_tables)),
  tar_target(mental_health_data, get_mental_health(mental_health_tables)),
  tar_target(ksads_y_data, get_ksads_y(ksad_tables)),
  tar_target(ksads_p_data, get_ksads_p(ksad_tables)),
  tar_target(physical_health_data, get_physical_health(physical_health_tables)),

  tar_target(calculated_scores_data, get_calculated_scores(calculated_scores, merged_demographics_general_data)),

  ### Before and after covid - choose 1 closet timepoint
  tar_target(abcd_general_pre_post_covid_tp, get_pre_post_covid_tp(abcd_general_data)),

  # merge
  tar_target(merged_data,
             merge_all_data(tp_include = tp_include
                            , merged_demographics_general_data
                            , culture_environment_data
                            , linked_external_data
                            , mental_health_data
                            , ksads_y_data
                            , ksads_p_data
                            , physical_health_data
                            , calculated_scores_data
                            , abcd_general_pre_post_covid_tp
             )),

  # Impute main study
  tar_target(imputed_data, impute_main_study_data(merged_data)),

  # Add depression variables
  tar_target(abcd_data_dep_score, calculate_dep_vars_abcd(merged_data)),
  tar_target(imputed_abcd_data_dep_score, calculate_dep_vars_abcd(imputed_data)),


  #### COVID STUDY ####
  ## Covid data
  tar_target(covid_19_data, get_covid_19(covid_19_tables)),

  ## calculate covid age
  tar_target(covid_19_data_age, calculate_covid_age(covid_19_data, merged_data)),

  ## Impute covid without demographics
  tar_target(imputed_covid_19_data, impute_covid(covid_19_data_age)),

  # Add depression variables
  tar_target(covid_data_dep_score, calculate_dep_vars_covid(covid_19_data_age)),
  tar_target(imputed_covid_dep_score, calculate_dep_vars_covid(imputed_covid_19_data)),


  #### Combine Studies ####
  tar_target(whole_data_wide, create_whole_data(abcd_data_dep_score, covid_data_dep_score)),
  tar_target(imputed_whole_data_wide, create_whole_data(imputed_abcd_data_dep_score, imputed_covid_dep_score, imputed = T)),

  tar_target(stratified_data, statify_data(merged_data, whole_data_wide), format = "file")


)


















