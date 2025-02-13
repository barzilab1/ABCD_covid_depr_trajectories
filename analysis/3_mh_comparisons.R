source("analysis/analysis_utility_fun.R")
source("configurations/path_config.R")

# Figure 2, Panel A: trajectory plot of main depression symptoms
trajectory_data <- file.path(project_path, "mplus_outputs/4bins_postC_3_classes_no_covar_random_int_depm05_NOTign_121724.gh5")
fig2a <- trajectory_plot(filepath = trajectory_data,order = c("Chronic","Susceptible","Resilient"))
fig2a_complete <- ggarrange(fig2a$trajectories + ylim(0,4),fig2a$prop,widths=c(2,1))

# eTable 5: slopes and intercepts by trajectory class
sjPlot::tab_model(lm(data = fig2a$plot_data %>% filter(group == "Resilient"), value ~ bins_num),
                  lm(data = fig2a$plot_data %>% filter(group == "Susceptible"), value ~ bins_num),
                  lm(data = fig2a$plot_data %>% filter(group == "Chronic"), value ~ bins_num),
                  show.se = T, show.ci = F)

# Figure 2, Panel B:
# Data preparation
data <- read.csv(file.path("outputs","data_wide_4bins_class.csv"))

# Select depression scores only
data_depression <- select(data,starts_with("depression") | c("class_name"))
colnames(data_depression) <- colnames(data_depression) %>%
  gsub("main_","main",.) %>%
  gsub("sens_","sens",.) %>%
  gsub("0","preCovid",.) %>%
  gsub("5","postCovid",.)
data_prepost <- data_depression[c("class_name","depression_main_preCovid", "depression_main_postCovid")]
data_long <- tidyr::pivot_longer(data_prepost, cols = c(depression_main_preCovid, depression_main_postCovid),
                                 names_to = "variable", values_to = "score")
data_long$variable = factor(data_long$variable, levels = c("depression_main_preCovid", "depression_main_postCovid"))
data_long$score = as.double(data_long$score)
data_long$class_name = factor(data_long$class_name, levels = c("Resilient", "Susceptible","Chronic"))

# Figure 2, Panel B: Mental health comparisons pre- and post-COVID

fig2b <- ggplot(data_long, aes(x = class_name, y = score, fill = variable)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, notch = TRUE) +
  labs(x = "", y = "No. of depression symptoms", fill = "Timepoint") +
  scale_fill_manual(values = c("forestgreen", "darkred"),labels = c("Pre-pandemic","Post-pandemic")) +
  scale_x_discrete(labels = c("Chronic" = "Chronic (N = 159)",
                              "Resilient" = "Resilient (N = 3,027)",
                              "Susceptible" = "Susceptible (N = 326)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        legend.position = "top",
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13))
# Arrange panel A
fig2a_complete <- ggarrange(fig2a$trajectories + ylim(0,4) + guides(color = "none") ,fig2a$prop, widths=c(2,1), common.legend = T)

# Combine figure 2: merged panel A and panel B
fig2 <- ggarrange(fig2a_complete, fig2b , ncol = 1, heights = c(5,4), labels = "AUTO")
ggsave(filename="results/manuscript/figure2.pdf",
       width = 8, height = 9,
       plot = fig2)

# eTable 6: Comparisons of resilient and susceptible groups by diagnoses (MDD & others)
# Select K-SADS and BPM diagnosis and symptom dimension scores
data_twoclass_extended <- data_twoclass %>% 
  filter(class_name %in% c("Resilient","Susceptible")) %>%
  select((matches("^bpm|ksads") & matches("_postCovid")) | c("class_name")) %>%
  select(-matches("t_postCovid"))
mh_comparisons_supplement <- data_twoclass_extended %>%
  tbl_summary(by = "class_name",
              statistic = list(all_continuous() ~ "{mean} ({sd}",
                               all_dichotomous() ~ "{n} ({p}%)"),
              type = list(where(is.logical) ~ "dichotomous"),
              missing = "ifany") %>%
  add_p()

# Save comparison table
mh_comparisons_supplement %>%
  as_gt() %>%
  gt::gtsave(filename="results/manuscript/Supplemental_Table6_mh_comparisons_main.docx")


