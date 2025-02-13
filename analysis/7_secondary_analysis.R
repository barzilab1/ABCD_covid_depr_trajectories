source("analysis/analysis_utility_fun.R")
source("configurations/path_config.R")

# eTable 3: Depression score missingness
data <- read.csv(file.path("data","data_long_4bins.csv"))

data <- data %>%
  select(matches("src_subject_id|depression_m|bins"))

for (i in 1:4){
  cvd_count <- data %>%
    filter(bins == i)
  count = length(unique(cvd_count$src_subject_id))
  cvd_count <- cvd_count %>%
    dplyr::group_by(src_subject_id) %>%
    dplyr::summarize(
      across(matches("[1-8]"),
             ~ all(is.na(.)),
             .names = "MDD_A{stringr::str_sub(.col, 13, 13)}"),
      .groups = "drop"  # Avoids adding extra grouping attributes
    ) %>%
    select(-src_subject_id) %>%
    dplyr::summarize(across(everything(), sum, na.rm = TRUE))
  print(paste0("Bin ",i))
  print(cvd_count[1,])
  print(cvd_count[1,]/count*100)
}

pre <- data[data$bins == 0,]
post <- data[data$bins == 5,]

pre_missing_counts <- pre %>%
  select(-src_subject_id) %>%
  dplyr::summarize(across(everything(), ~ sum(is.na(.)))) %>%
  select(matches("depression_m[1-8]")) %>%
  mutate(timepoint = "Pre-pandemic")

post_missing_counts <- post %>%
  select(-src_subject_id) %>%
  dplyr::summarize(across(everything(), ~ sum(is.na(.)))) %>%
  select(matches("depression_m[1-8]")) %>%
  mutate(timepoint = "Post-pandemic")

missingness <- rbind(pre_missing_counts,post_missing_counts) %>%
  rename_with(~ gsub("^depression_m", "MDD_A", .), starts_with("depression_m")) %>%
  rbind(cvd_missing_counts)
missingness_percentages <- missingness %>% mutate(across(matches("MDD"), ~round(.x*100 / length(unique(cvd$src_subject_id)), 1)))

# eFigures 1 & 2: imputed and sensitive trajectories
path_figs1 <- file.path(project_path, "mplus_outputs/4bins_postC_3_classes_no_covar_random_int_deps05_NOTign_121724.gh5")
figs1 <- trajectory_plot(filepath = path_figs1,order = c("Resilient","Susceptible","Chronic"))
figs1_complete <- ggarrange(figs1$trajectories + ylim(0,4.5),figs1$prop,widths=c(2,1))
ggsave(filename="results/manuscript/efigure_1.pdf",width=8,height=5,
       plot=figs1_complete)

path_figs2 <- file.path(project_path, "mplus_outputs/4bins_postC_3_classes_no_covar_random_int_depm05_NOTign_imp_121724.gh5")
figs2 <- trajectory_plot(filepath = path_figs2,order = c("Chronic","Susceptible","Resilient"))
figs2_complete <- ggarrange(figs2$trajectories + ylim(0,4),figs2$prop,widths=c(2,1))
ggsave(filename="results/manuscript/efigure_2.pdf",width=8,height=5,
       plot=figs2_complete)


# eTable 19
sjPlot::tab_model(lm(data = figs1$plot_data %>% filter(group == "Resilient"), value ~ bins_num),
                  lm(data = figs1$plot_data %>% filter(group == "Susceptible"), value ~ bins_num),
                  lm(data = figs1$plot_data %>% filter(group == "Chronic"), value ~ bins_num),
                  show.se = T)

# eTable 20
sjPlot::tab_model(lm(data = figs2$plot_data %>% filter(group == "Resilient"), value ~ bins_num),
                  lm(data = figs2$plot_data %>% filter(group == "Susceptible"), value ~ bins_num),
                  lm(data = figs2$plot_data %>% filter(group == "Chronic"), value ~ bins_num),
                  show.se = T)
