############# file paths #############

#path to abcd Restricted Access folder
root = "~"
box_path = "Library/CloudStorage/Box-Box"
abcd_box_path = "2. Barzi Lab - Restricted Access/2-ABCD"
abcd_data_path = "ABCD data"
abcd_version = "5.1"
abcd_core = "core"
abcd_sub = "substudies"
prs_box_path = "Ran_Barzilay"

# ~/Box Sync/2. Barzi Lab - Restricted Access/2-ABCD/ABCD data/5.0/core/abcd-general
abcd_data = file.path(root, box_path, abcd_box_path, abcd_data_path)
additional_files_path = file.path(root, box_path, abcd_box_path,"Additional files")
abcd_core_path = file.path(abcd_data, abcd_version, abcd_core)
abcd_sub_path = file.path(abcd_data, abcd_version, abcd_sub)

# file path for each core domain
abcd_core_general_path = file.path(abcd_core_path, "abcd-general")
abcd_core_culture_environment_path = file.path(abcd_core_path, "culture-environment")
abcd_core_led_path = file.path(abcd_core_path, "linked-external-data")
abcd_core_mental_health_path = file.path(abcd_core_path, "mental-health")
abcd_core_neurocognition_path = file.path(abcd_core_path, "neurocognition")
abcd_core_physical_health_path = file.path(abcd_core_path, "physical-health")

# file path for each substudies domain
abcd_sub_covid19_path = file.path(abcd_sub_path, "covid-19")

# file path for polygenic risk scores
abcd_genetics_path = file.path(abcd_data, "genetics")
abcd_genetics_files_path = file.path(abcd_genetics_path, "genetic.csv")

# file path for exposome scores
exposome_tyler = "Projects/exposome_Tyler/3.0/results"
e_factor_files_path = file.path(root, box_path, abcd_box_path, exposome_tyler, "e factor scores")

# project-specific file paths
project_path = file.path(root, box_path, abcd_box_path, "Projects/Psych_resilience_trajectories")

# ksads 1 & 2 coalesce files
ksads_coalesce_path = file.path(additional_files_path, "ksad")

