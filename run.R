#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

# # in case of more than one dataset file
# targets::tar_config_set(script = "_targets.R", store = "_targets", project = "main")
# targets::tar_config_set(script = "_targets_grant.R", store = "_grant", project = "grant")
# # use the relevant environment
# Sys.setenv(TAR_PROJECT = "main")
# Sys.setenv(TAR_PROJECT = "grant")

targets::tar_make()


targets::tar_visnetwork(targets_only = T)
targets::tar_visnetwork()

targets::tar_meta(fields = error, complete_only = TRUE)
targets::tar_meta(fields = warnings, complete_only = TRUE)
View(targets::tar_meta(targets_only = T))


# Debugging 
# targets::tar_read(merged_demographics_general_data)
# targets::tar_read(culture_environment_data)
# targets::tar_read(mental_health_data)
# targets::tar_read(suicide_y_data)
# targets::tar_read(physical_health_data)



