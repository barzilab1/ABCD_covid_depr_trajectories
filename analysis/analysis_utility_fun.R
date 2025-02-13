library(psych)
library(Hmisc)
library(tidyverse)
library(reshape)
library(rhdf5)
library(tableone)
library(gtsummary)
library(stats)
library(ggpubr)
library(ggstatsplot)
library(lme4)
source("analysis/mplus.R")


# Create three-group trajectory plot with picked colors and figure labels and stacked bar chart for group distribution
trajectory_plot <- function(filepath, order) {
  # Read in Mplus model
  fig_df <- data.frame(mplus.get.sample_means(filepath, 'process1'))

  # Set default settings for plots
  bins_list = c("Pre-Mar 2020","Mar – Jul 2020","Aug – Nov 2020",
                "Dec 2020 – Apr 2021","May – Jul 2021","Post-Jul 2021")
  bins_num = 1:6
  final_order = c("Resilient","Susceptible","Chronic")
  colnames(fig_df) <- order

  # TODO change order. all the code related to fig_df in one batch

  # Extract group distribution - counts and percentages
  freq_df <- data.frame(table(mplus.get.data(filepath, 'class'))) %>%
    mutate(classes = factor(order,levels=final_order),
           Freq = as.numeric(Freq),
           Percentage = round(100 * Freq / sum(Freq), 1),
           Label = paste0(Freq, " (", Percentage, "%)")) %>%
    select(-Var1) %>%
    arrange(desc(classes)) %>%
    mutate(prop = Freq / sum(Freq),
           cumulative = cumsum(prop),
           mid_point = cumulative - 0.5 * prop)

  # Convert data to long form plotting
  fig_df$bins <- bins_list
  fig_df$bins_num <- bins_num
  # fig_df <- select(fig_df,c("bins", "bins_num", "Resilient", "Susceptible", "Chronic"))
  fig_df_long <- melt(fig_df, id = c("bins", "bins_num"), variable_name = "group")
  fig_df_long <- fig_df_long %>%
    mutate(bins = factor(bins,levels=bins_list),
           group = factor(group,levels=final_order))

  # Trajectory plot of six timepoints for three classes
  fig <- ggplot(data=fig_df_long,aes(x=bins,y=value,color=group,group=group)) +
    geom_line(linewidth = 1.5) +
    geom_point() +
    labs(x = "", y = "No. of depression symptoms", color = "") +
    ylim(0,3.5) +
    theme_minimal() +
    scale_color_manual(values = c("#009E73", "#E69F00", "#CC79A7")) +
    theme(axis.text.x = element_text(angle = 50, hjust=1, size = 13, color = "black"),
          axis.text.y = element_text(size = 13, color = "black"),
          axis.title.y = element_text(size = 14),
          legend.position = "none")

  # Stacked bar chart of the distribution of all three classes
  fig_dist <- ggplot(data=freq_df,aes(x="Sample",y=Freq,fill=factor(classes))) +
    geom_bar(stat="identity",position="stack",color="white") +
    labs(fill = "") +
    scale_fill_manual(values = c("#009E73", "#E69F00", "#CC79A7")) +
    # TODO why not to use Label from the dataframe? why was it created if not for that?
    geom_text(aes(label=paste0(Freq," (",sprintf("%1.1f", Percentage),"%)")),
              position=position_stack(vjust=0.5)) +
    theme_void() +
    theme(legend.text=element_text(size=13))

  # Store ggplot2 objects in a list and return
  return(list(trajectories = fig, prop = fig_dist, plot_data = fig_df_long))
}

# Runs logistic mixed effects regression (LMER) model (default: IV = risk, nested random effects, and age + sex as covariates)
run_logmodel <- function(IV, data, FE, DV = "risk", RE = "(1|site_id_l_br/rel_family_id)"){

  # Save formula in the form: IV ~ DV + FE covariates + nested random effects
  if(is.na(FE)) fx = as.formula(paste0(DV,"~",IV,"+",RE)) else fx = as.formula(paste0(DV,"~",IV,"+",FE,"+",RE))

  # Run LMER model using saved formula
  model <- glmer(formula = fx,data=data,family=binomial,nAGQ=0)
  return(model)
}

# TOOD: map/list function
# Extracts key statistics from model: odds ratio for DV of interest, 95% CIs, p-value
add_or <- function(main_df, model, var, type){

  # Odds ratio
  or = round(exp(fixef(model)[var]),digits=2)

  # Confidence intervals
  cis <- as.data.frame(confint(model,parm="beta_",method="Wald"))
  or_2.5 = round(exp(cis[var,1]),digits=2)
  or_97.5 = round(exp(cis[var,2]),digits=2)

  # P-value
  pval = coef(summary(model))[var,4]

  # Set model type (to assign subgroup in figure, e.g., cyberbullying in peer)
  model_type = type

  # Save as a row in a data frame with OR, 95% CI, and p-values
  main_df[nrow(main_df) + 1, ] <- c(var, or, or_2.5, or_97.5, pval, model_type)
  return(main_df)
}

# Performing G-by-E analysis: exposome (E) only, genetics (G) only, E + G, and E x G
gxe_analysis <- function(PRS, df, ERS = "exposome_score_1y_preCovid") {
  g_model <- run_logmodel(DV = PRS,FE = "age_Mar2020 + sex_br",data = df) # Polygenic risk only
  e_model <- run_logmodel(DV = ERS,FE = "age_Mar2020 + sex_br",data = df) # Exposomic risk only
  ge_model <- run_logmodel(DV = paste0(ERS," + ",PRS),FE = "age_Mar2020 + sex_br",data = df) # Additive effects model
  g_by_e_model <- run_logmodel(DV = paste0(ERS," * ",PRS),FE = "age_Mar2020 + sex_br",data = df) # Interaction effects model
  return(list(anova_gpluse = anova(g_model,ge_model), # Chi-squared model difference tests
              anova_eplusg = anova(e_model,ge_model),
              genetic_model = g_model,
              exp_model = e_model,
              genexp_model = ge_model,
              intx_model = g_by_e_model))
}

