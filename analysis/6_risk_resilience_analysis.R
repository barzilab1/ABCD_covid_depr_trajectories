library(sjPlot)
library(ggstatsplot)
library(ggridges)

source("analysis/analysis_utility_fun.R")
source("configurations/path_config.R")

# Initialize data frame to store OR values, 95% CI bounds, and p-values
or_main_models <- data.frame(
  model = character(),
  or = numeric(),
  or_2.5 = numeric(),
  or_97.5 = numeric(),
  pval = numeric(),
  model_type = character()
)

# Load re-organized risk and resilience factor data (see 7_pubertal_analyses.R)
df_2class_main = read.csv(file="outputs/df_risk_resilience_factors.csv")

# Logistic mixed effects models
# Z-scoring individual-level continuous raw scores
scalable_vars <- c("fes_y_ss_fc_preCovid","pmq_y_ss_mean_preCovid",
                   "psb_y_ss_mean_preCovid","wps_ss_sum_preCovid")
df_2class_main[,scalable_vars] <- scale(df_2class_main[,scalable_vars])
covars = "age_Mar2020 + sex_br"

# Puberty models: late puberty, youth-reported pubertal stage
## eTable8
puberty <- run_logmodel(IV="late_pubertal",FE = NA, data = df_2class_main)
puberty_sex <- run_logmodel(IV="late_pubertal",FE = "sex_br",data = df_2class_main)
puberty_sex_intx <- run_logmodel(IV="late_pubertal * sex_br",FE = NA, data = df_2class_main)

## eTable9
ctn_puberty <- run_logmodel(IV="puberty_both_sexes",FE = NA, data = df_2class_main)
ctn_puberty_sex <- run_logmodel(IV="puberty_both_sexes",FE = "sex_br",data = df_2class_main)
ctn_puberty_sex_intx <- run_logmodel(IV="puberty_both_sexes * sex_br",FE = NA, data = df_2class_main)

tab_model(puberty,puberty_sex,puberty_sex_intx,
          file="results/manuscript/Supplemental_Table8_puberty_sex.xls")
tab_model(ctn_puberty,ctn_puberty_sex,ctn_puberty_sex_intx,
          file="results/manuscript/Supplemental_Table9_continuousvar_puberty_sex.xls")

# Add odds ratio, confidence intervals, and p-value to table for plotting figure 4
or_main_models <- add_or(or_main_models,df_2class_main,puberty_sex,"late_pubertal","Bio")

# Trait resilience: prosocial behaviors, problem-solving skills
prosocial <- run_logmodel(IV="psb_y_ss_mean_preCovid",FE = covars,data = df_2class_main)
problemsolve <- run_logmodel(IV="wps_ss_sum_preCovid",FE = covars,data = df_2class_main)

tab_model(prosocial,problemsolve,
          file="results/manuscript/Supplemental_Table11_trait.xls")

or_main_models <- add_or(or_main_models,df_2class_main,prosocial,"psb_y_ss_mean_preCovid","Trait")
or_main_models <- add_or(or_main_models,df_2class_main,problemsolve,"wps_ss_sum_preCovid","Trait")

# Family conflict & parental monitoring
conflict <- run_logmodel(IV="fes_y_ss_fc_preCovid",FE = covars,data = df_2class_main)
monitoring <- run_logmodel(IV="pmq_y_ss_mean_preCovid",FE = covars,data = df_2class_main)

or_main_models <- add_or(or_main_models,df_2class_main,conflict,"fes_y_ss_fc_preCovid","Home environment")
or_main_models <- add_or(or_main_models,df_2class_main,monitoring,"pmq_y_ss_mean_preCovid","Home environment")

tab_model(conflict,monitoring,
          file="results/manuscript/Supplemental_Table12_homeenv.xls")


# General SES variables: Household income, ADI, exposome score
#TODO household_income is integer. no need to convert it
household_SES <- run_logmodel(IV="household_income",FE = covars,data = df_2class_main)
adi <- run_logmodel(IV="reshist_addr1_adi_wsum_preCovid",FE = covars,data = df_2class_main)
exposome <- run_logmodel(IV="exposome_score_1y_preCovid",FE = covars,data = df_2class_main)
exposome_added <- run_logmodel(IV="exposome_score_1y_preCovid", FE = "household_income + reshist_addr1_adi_wsum_preCovid + age_Mar2020 + sex_br",data = df_2class_main)
tab_model(household_SES,adi,exposome,exposome_added,
          file="results/manuscript/Supplemental_Table13_exposome.xls")

or_main_models <- add_or(or_main_models,df_2class_main,household_SES,"household_income","SES")
or_main_models <- add_or(or_main_models,df_2class_main,adi,"reshist_addr1_adi_wsum_preCovid","SES")
or_main_models <- add_or(or_main_models,df_2class_main,exposome,"exposome_score_1y_preCovid","General adversity")


# Social environment: (cyber)bullying
cyber <- run_logmodel(IV="cybb_phenx_harm_preCovid",FE = covars,data = df_2class_main)
peer_victimz <- run_logmodel(IV="bully_vic_90_q_preCovid",FE = covars,data = df_2class_main)
cyber_bully <- run_logmodel(IV="cybb_phenx_harm_preCovid",FE = "bully_vic_90_q_preCovid + age_Mar2020 + sex_br",data = df_2class_main)

tab_model(cyber,peer_victimz,cyber_bully,
          file="results/manuscript/Supplemental_Table14_bullying.xls")

or_main_models <- add_or(or_main_models,df_2class_main,cyber,"cybb_phenx_harm_preCovid","Peers")
or_main_models <- add_or(or_main_models,df_2class_main,peer_victimz,"bully_vic_90_q_preCovid","Peers")


# Parental history of depression
motherhx_mdd <- run_logmodel(IV="famhx_ss_moth_prob_dprs_p_preCovid",FE = covars,data = df_2class_main)
fatherhx_mdd <- run_logmodel(IV="famhx_ss_fath_prob_dprs_p_preCovid",FE = covars,data = df_2class_main)
tab_model(motherhx_mdd,fatherhx_mdd,file="results/manuscript/Supplemental_Table15_parent_mh_hx.xls")

or_main_models <- add_or(or_main_models,df_2class_main,motherhx_mdd,"famhx_ss_moth_prob_dprs_p_preCovid","Family history")
or_main_models <- add_or(or_main_models,df_2class_main,fatherhx_mdd,"famhx_ss_fath_prob_dprs_p_preCovid","Family history")
# or_main_models <- add_or(or_main_models,df_2class_main,parenthx_mdd,"famhx_ss_momdad_dprs_p_preCovid","Family history")


# MDD polygenic risk for European (EUR) and African (AFR) genetic ancestry separately
mdd_prs_eur <- run_logmodel(IV="MDD_PRS",FE = covars,data = df_2class_main)
# mdd_prs_eur <- run_logmodel(IV="MVP_MDD_PRS",data = df_2class_main %>% dplyr::filter(genetic_afr == 0)) # check which variable to use for EUR
mdd_prs_afr <- run_logmodel(IV="MVP_MDD_PRS",FE = covars,data = df_2class_main %>% dplyr::filter(genetic_afr == 1))
# df_2class_main %>% select(matches("src|MDD_PRS|genetic_afr")) %>% View()

tab_model(mdd_prs_eur,mdd_prs_afr,
          file="results/manuscript/Supplemental_Table16_genetics.xls")

or_main_models <- add_or(or_main_models,df_2class_main,mdd_prs_eur,"MDD_PRS","Bio") # check
or_main_models <- add_or(or_main_models,df_2class_main,mdd_prs_afr,"MVP_MDD_PRS","Bio")


# Interaction analyses: figure 4C
# Filtering EUR and AFR ancestry for additive and interaction analyses
eur_genetic_main <- df_2class_main %>% filter(!is.na(MDD_PRS) & genetic_afr == 0 & !is.na(exposome_score_1y_preCovid))
afr_genetic_main <- df_2class_main %>% filter(!is.na(MVP_MDD_PRS) & genetic_afr == 1 & !is.na(exposome_score_1y_preCovid))

# G, G+E, E, GxE model (saving models as tables for supplement reporting)
eur_mdd <- gxe_analysis(PRS = "MDD_PRS",df=eur_genetic_main)
tab_model(eur_mdd$genetic_model,eur_mdd$exp_model,eur_mdd$genexp_model,eur_mdd$intx_model,
          file="results/manuscript/Supplemental_Table17_mdd_eur.xls")

afr_mdd <- gxe_analysis(PRS = "MVP_MDD_PRS",df=afr_genetic_main)
tab_model(afr_mdd$genetic_model,afr_mdd$exp_model,afr_mdd$genexp_model,afr_mdd$intx_model,
          file="results/manuscript/Supplemental_Table18_mdd_afr.xls")

#TODO why we need it both here and in the function?
anova(eur_mdd$genetic_model, eur_mdd$genexp_model, test="Chisq")
anova(eur_mdd$exp_model, eur_mdd$genexp_model, test="Chisq")

# Figure 4AB: Environmental and biological contributors to probability of risk
# Colorblindness friendly palette for visualizing multiple IV types
cbb_palette <- cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#CC79A7")

# Prepare OR table (with data extracted from each relevant LMEM)
or_main_models[,c(2:5)] <- lapply(or_main_models[,c(2:5)], as.numeric)
or_main_models$model_type = factor(or_main_models$model_type,levels = c("Trait","Family history","Peers","Home environment","SES","General adversity","Bio"))
or_main_models <- or_main_models %>% arrange(model_type)
or_main_models$model <- factor(or_main_models$model, levels = or_main_models$model)
or_main_models$sig <- case_when(or_main_models$pval < 0.001 ~ "***",
                                or_main_models$pval < 0.01 ~ "**",
                                or_main_models$pval < 0.05 ~ "*",
                                TRUE ~ "")
or_main_models$model <- c("Prosocial (z)",
                          "Problem-solving (z)",
                          "Mother MDD (Y/N)",
                          "Father MDD (Y/N)",
                          "Cyberbullying (Y/N)",
                          "Victimization (Y/N)",
                          "Conflict (z)",
                          "Monitoring (z)",
                          "Income (decile)",
                          "ADI (raw)",
                          "Exposome (z)",
                          "Late puberty",
                          "MDD PRS [EUR]",
                          "MDD PRS [AFR]")

# 4A: environmental contributors
df_fig4a <- or_main_models %>% dplyr::filter(model_type != "Bio")
fig4a <- ggplot(df_fig4a, aes(x = or, y = model, color = model_type)) +
  geom_point(size = 3) + # OR
  geom_errorbarh(aes(xmin = or_2.5, xmax = or_97.5), # Confidence intervals
                 height = 0.2, size = 1) +
  geom_text(aes(label = sig),  # Position significance asterisks above OR value
            vjust = 0.1, size = 7) +
  facet_grid(model_type ~ ., scales = "free", space = "free", switch = "y") + # Organize by IV category
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_continuous(trans = 'log10',limits=c(0.5,4)) +
  labs(x = "Odds Ratio (95% CI)", y = "") +
  scale_colour_manual(values=cbb_palette) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12,color = "black"),
        axis.text.x = element_text(size = 12,color = "black"),
        axis.title.x = element_text(size = 14),
        strip.text.y.left = element_text(angle=0, size = 12,color = "black"),
        strip.background = element_rect(color = "black",
                                        linewidth = 1),
        strip.placement = "outside",
        legend.position = "none")

# 4B: biological contributors
df_fig4b <- or_main_models %>% dplyr::filter(model_type == "Bio")
fig4b <- ggplot(df_fig4b, aes(x = or, y = model)) +
  geom_point(size = 3,
             color = cbb_palette[1:3]) + # OR
  geom_errorbarh(aes(xmin = or_2.5, xmax = or_97.5), # Confidence intervals
                 height = 0.2, size = 1,
                 color = cbb_palette[1:3]) +
  geom_text(aes(label = sig),  # Position significance asterisks above OR value
            vjust = 0.1, size = 7,
            color = cbb_palette[1:3]) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_continuous(trans = 'log10') +
  labs(x = "Odds Ratio (95% CI)", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12,color = "black"),
        axis.text.x = element_text(size = 12,color = "black"),
        axis.title.x = element_text(size = 14))

# Figure 4C: G-by-E interaction
fig4c <- plot_model(eur_mdd$intx_model, type = "pred", terms = c("MDD_PRS [all]","exposome_score_1y_preCovid [meansd]"),
                    show.values = TRUE, show.intercept = FALSE, ci_level=0.95,
                    legend.title = "Exposomic burden", line.size=1.25) + # Prediction probability plot, stratified by exposome burden
  scale_color_manual(labels = c("Low (-1 SD)", "Medium (mean)", "High (+1 SD)"), # Strata of exposome score (mean+/-SD)
                     values = cbb_palette[c(4,6,8)]) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text = element_text(size=12,color="black"),
        axis.title = element_text(size=14,color="black"),
        legend.title = element_text(size=11,color="black"),
        legend.text = element_text(size=10,color="black"),
        title = element_blank()) +
  labs(x = "MDD polygenic risk", y = "% probability of risk")

# Merge panels B & C (stacked vertically)
fig4bc <- ggarrange(fig4b,fig4c,ncol=1,labels=c("B","C"))

# Merge panel A (left) and panels B & C (right)
fig4 <- ggarrange(fig4a,fig4bc,ncol=2,labels=c("A",""))
ggsave(filename="results/manuscript/figure4.pdf",
       width=11.5,height=7)
