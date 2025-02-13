source("analysis/analysis_utility_fun.R")
source("configurations/path_config.R")

# Create cleaned risk and resilience factor dataset
data_main <- read.csv(file.path("outputs","data_wide_4bins_class.csv"))
# TODO change colnames(data_main)[grepl("race_", colnames(data_main))] to: grep("race_",colnames(data_main), value = T)
# TODO why we need to separate binary and factor variables?
binary_cols = c(colnames(data_main)[grepl("race_",colnames(data_main))],"sex_br","resilient","risk")
factor_cols = c("household_income")

# Reformat the data, recode income into four bins as SES, factorize, etc.
data_main = data_main %>%
  mutate_at(c(binary_cols,factor_cols),as.factor)

#TODO use class_name instead of class. the class values might change every time Kate run the models,
# using class_names eliminates the need to fix this code
df_2class_main = data_main[data_main$class_name != "Chronic",]
#TODO why we need to save this? it keeps only the filtering
write.csv(df_2class_main,file=file.path("outputs","df_risk_resilience_factors.csv"),row.names = FALSE)

# Figure 3, Panel A: Contingency table of puberty status comparisons
fig3a <- ggbarstats(data = df_2class_main, y = class_name, x = late_pubertal, label = "both",
                    bf.message = FALSE, results.subtitle = FALSE, xlab = "", legend.title = "Pubertal Stage") +
  labs(subtitle = NULL) +
  scale_fill_discrete(labels = c("Late","Early")) +
  theme(legend.position = "bottom",
        text = element_text(size = 14),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

# ggsave(filename="results/manuscript/figure3a_stackedbars.pdf",
#        width = 5, height = 6,
#        plot = fig3a)

# Chi-square comparison test
# TODO use class_name instead of risk
chisq.test(df_2class_main$risk, df_2class_main$late_pubertal)


# Figure 3, Panel B & C: Trajectories stratified by puberty (post-COVID early puberty/pre-COVID late puberty)
path_fig3b <- file.path(project_path, "mplus_outputs/4bins_postC_3_classes_no_covar_random_int_depm05_NOTign_PrP_PoC_121724.gh5")
path_fig3c <- file.path(project_path, "mplus_outputs/4bins_postC_3_classes_no_covar_random_int_depm05_NOTign_PoP_PrC_121724.gh5")

fig3b <- trajectory_plot(filepath = path_fig3b, order = c("Resilient","Chronic","Susceptible"))
fig3c <- trajectory_plot(filepath = path_fig3c, order = c("Resilient","Chronic","Susceptible"))

Post_pub_pre_COVID_N <- read_csv("data/b4_NOTign_PoP_PrC.csv") %>% nrow()
Pre_pub_post_COVID_N <- read_csv("data/b4_NOTign_PrP_PoC.csv") %>% nrow()

# Add title to trajectory plot of panel B
fig3b_trajectories <- fig3b$trajectories +
  ggtitle(paste0("Early pubertal post-July 2021 [N = ", Pre_pub_post_COVID_N, "]")) +
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  guides(color = "none")

fig3b_prop <- fig3b$prop +
  theme(legend.key = element_rect(color="black",linewidth = 1))

# Merge with bar graph
fig3b_merged <- ggarrange(plotlist = list(fig3b_trajectories,fig3b_prop),widths=c(2,1),
                          common.legend=TRUE,legend = "bottom")

# Add title to trajectory plot of panel C
fig3c_trajectories <- fig3c$trajectories +
  ggtitle(paste0("Late pubertal pre-March 2020 [N = ", Post_pub_pre_COVID_N, "]")) +
  theme(plot.title = element_text(hjust = 0.5, size=15)) +
  guides(color = "none")

# Merge with bar graph
fig3c_merged <- ggarrange(plotlist = list(fig3c_trajectories,fig3c$prop),widths=c(2,1),
                          common.legend=TRUE,legend = "none")

# Merge panels B & C
fig3bc <- ggarrange(fig3b_merged,fig3c_merged,ncol=1,labels=c("B","C"))

# Fully arrange panels A with panels B & C, save
fig3 <- ggarrange(fig3a,fig3bc,ncol=2,widths=c(1,2),labels=c("A",""))
ggsave(filename="results/manuscript/figure3.pdf",width=12,height=10,plot=fig3)

# eTable 10 # Bottom
sjPlot::tab_model(lm(data = fig3b$plot_data %>% filter(group == "Resilient"), value ~ bins_num),
                  lm(data = fig3b$plot_data %>% filter(group == "Susceptible"), value ~ bins_num),
                  lm(data = fig3b$plot_data %>% filter(group == "Chronic"), value ~ bins_num),
                  show.se = T, show.ci = F)

# eTable 10 # Top
sjPlot::tab_model(lm(data = fig3c$plot_data %>% filter(group == "Resilient"), value ~ bins_num),
                  lm(data = fig3c$plot_data %>% filter(group == "Susceptible"), value ~ bins_num),
                  lm(data = fig3c$plot_data %>% filter(group == "Chronic"), value ~ bins_num),
                  show.se = T, show.ci = F)

