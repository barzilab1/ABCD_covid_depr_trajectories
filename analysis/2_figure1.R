library(tidyverse)
library(ggridges)
library(scales)
library(ggpubr)

source("configurations/path_config.R")

# Read in data
df_long <- read_csv("data/data_long_4bins.csv") %>%
  select(src_subject_id, eventname, interview_date, bins)

# Select COVID-only bins (first to fourth), rename collection wave names to readable, plot-appropriate values
df_long_cv <- df_long %>%
  dplyr::filter(!is.na(bins) & bins %in% 1:4) %>%
  mutate(eventname = str_replace(eventname, "covid19_cv(\\d+)_arm_2", "CV\\1"),
         eventname = str_replace(eventname, "_year_follow_up_y_arm_1", "Y FU"))

eventname_counts <- df_long_cv %>%
  group_by(eventname) %>%
  tally() %>%
  ungroup()

eventname_mapping <- eventname_counts %>%
  mutate(mapped_name = paste0(eventname, " [", n, "]")) %>%
  select(eventname, mapped_name) %>%
  tibble::deframe()

unmapped_eventnames <- setdiff(unique(df_long_cv$eventname), names(eventname_mapping))
if (length(unmapped_eventnames) > 0) {
  warning("The following eventname values are not mapped: ", paste(unmapped_eventnames, collapse = ", "))
}

df_long_cv$eventname <- eventname_mapping[df_long_cv$eventname]

# Create the plot: set bin thresholds as dashed lines, plot ridge lines per each data collection wave
bin_dates <- as.Date(c("2020-03-01","2020-08-01","2020-12-01","2021-05-01","2021-07-31"))
fig1b_cv <- ggplot(df_long_cv, aes(y = factor(eventname), x = interview_date, fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2.5, alpha = 0.5) + # Ridge lines with gradient coloring
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %Y",
    limits = as.Date(c("2020-03-01","2021-07-31"))) +  # Selects time period and formats grid lines
  scale_fill_gradientn(colours = c("forestgreen","darkred")) + # Gradient (from pre- to post-COVID)
  theme_ridges() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.length.x = rel(1)) +
  geom_vline(xintercept = bin_dates, linetype = "dashed") # Delineating where each bin starts and ends

# Select pre-COVID bin (zeroth bin)
df_long_pre_cv <- df_long %>%
  dplyr::filter(!is.na(bins) & bins == 0)

# Create the plot: density plot witgh distribution of dates (independent of collection wave)
fig1a_precv <- ggplot(df_long_pre_cv, aes(x = interview_date)) +
  geom_bar(stat = "bin", binwidth = 14, fill = "forestgreen", color = "black", alpha = 0.6) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y",
    limits = as.Date(range(df_long_pre_cv$interview_date))) + # Min-max selection of limits (2016-12-28,2020-03-01)
  ylab("Count") +
  theme_ridges(center_axis_labels = TRUE) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 60,hjust=1),
        axis.line.x = element_line(color = "black", linewidth = 1),  # Thick line for x-axis
        axis.line.y = element_line(color = "gray", linewidth = 0.25),
        axis.ticks.x = element_line(color = "black"),  # Add ticks to x-axis
        axis.ticks.length.x = rel(1))

# Select post-COVID bin (fifth bin)
df_long_post_cv <- df_long %>%
  dplyr::filter(!is.na(bins) & bins == 5)

# Create the plot: density plot witgh distribution of dates (independent of collection wave)
fig1c_postcv <- ggplot(df_long_post_cv, aes(x = interview_date)) +
  geom_bar(stat = "bin", binwidth = 7, fill = "darkred", color = "black", alpha = 0.6) +
  scale_x_date(
    date_breaks = "1 months",
    date_labels = "%b %Y",
    limits = as.Date(c("2021-08-01","2022-02-01"))) +
  scale_y_continuous(position = "right") +
  ylab("Count") +
  theme_ridges(center_axis_labels = TRUE) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 60,hjust=1),
        axis.line.x = element_line(color = "black", linewidth = 1),  # Thick line for x-axis
        axis.line.y = element_line(color = "gray", linewidth = 0.25),
        axis.ticks.x = element_line(color = "black"),  # Add ticks to x-axis
        axis.ticks.length.x = rel(1))

table(df_long_pre_cv$eventname)
table(df_long_cv$eventname)
table(df_long_post_cv$eventname)

# Arrange figures parallel to each other - save as PDF
fig1abc <- ggarrange(fig1a_precv, fig1b_cv, fig1c_postcv, nrow = 1)
ggsave(filename = "results/manuscript/figure1abc_densities.pdf", width = 12.5, height = 3,
       plot = fig1abc)
