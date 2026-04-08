# ------------------------------------------------------------------------------
# AQLI 2026 – PM2.5 Trend Comparison Across Data Releases (Appendix Figure A.1)
# Description : Computes population-weighted national PM2.5 trends and compares 
#               them across multiple AQLI dataset releases (2016–2024) to assess 
#               revisions and consistency over time.
#
# Developed By: Purushottam Gupta
# Role        : Data Architect / Data Analyst, AQLI (University of Chicago)
# Contact     : guptap@uchicago.edu
# Dependencies: dplyr, readr, readxl, tidyr, ggplot2, stringr, ggthemes, scales
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Load helper script and historical datasets
# - Helper script contains common functions/themes
# - "color_YYYY" datasets represent historical AQLI releases
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Load Dependencies & Source Files
# ------------------------------------------------------------------------------
source("~/R/helper_function_2026.R")

# ------------------------------------------------------------------------------
# Appendix Figure A.1
# Objective: Compare population-weighted PM2.5 trends across AQLI datasets
# ------------------------------------------------------------------------------

# Define start column (common across all datasets)
pm_weighted_col_start <- "pm1998_weighted"

# ------------------------------------------------------------------------------
# 2024 Dataset (Latest)
# - Compute national population-weighted PM2.5 trend
# ------------------------------------------------------------------------------

pm_weighted_col_end <- "pm2024_weighted"

fig_a1_plt_part24 <- gadm2_aqli_2024 %>%
  filter(!is.na(population)) %>%                         # Ensure valid population
  mutate(country = "foo") %>%                            # Dummy grouping for national aggregation
  group_by(country) %>%
  mutate(
    pop_weights = population / sum(population, na.rm = TRUE),  # Population weights
    across(starts_with("pm"), ~ .x * pop_weights, .names = "{col}_weighted")
  ) %>%
  summarise(across(ends_with("weighted"), sum)) %>%      # Aggregate weighted PM
  pivot_longer(
    cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end),
    names_to = "years",
    values_to = "pop_weighted_avg_pm2.5"
  ) %>%
  mutate(
    years = as.integer(str_extract(years, "\\d+")),      # Extract year
    region = "National Average",
    ref_dataset = "2024 dataset"
  ) %>%
  select(years, region, pop_weighted_avg_pm2.5, ref_dataset)

# ------------------------------------------------------------------------------
# Repeat same logic for previous AQLI datasets
# Purpose: Compare revisions across releases
# ------------------------------------------------------------------------------

pm_weighted_col_end <- "pm2023_weighted"

fig_a1_plt_part23 <- gadm2_aqli_2023 %>%
  filter(!is.na(population)) %>%
  mutate(country = "foo") %>%
  group_by(country) %>%
  mutate(
    pop_weights = population / sum(population, na.rm = TRUE),
    across(starts_with("pm"), ~ .x * pop_weights, .names = "{col}_weighted")
  ) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(
    cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end),
    names_to = "years",
    values_to = "pop_weighted_avg_pm2.5"
  ) %>%
  mutate(
    years = as.integer(str_extract(years, "\\d+")),
    region = "National Average",
    ref_dataset = "2023 dataset"
  ) %>%
  select(years, region, pop_weighted_avg_pm2.5, ref_dataset)

pm_weighted_col_end <- "pm2022_weighted"

fig_a1_plt_part22 <- gadm2_aqli_2022 %>%
  filter(!is.na(population)) %>%
  mutate(country = "foo") %>%
  group_by(country) %>%
  mutate(
    pop_weights = population / sum(population, na.rm = TRUE),
    across(starts_with("pm"), ~ .x * pop_weights, .names = "{col}_weighted")
  ) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(
    cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end),
    names_to = "years",
    values_to = "pop_weighted_avg_pm2.5"
  ) %>%
  mutate(
    years = as.integer(str_extract(years, "\\d+")),
    region = "National Average",
    ref_dataset = "2022 dataset"
  ) %>%
  select(years, region, pop_weighted_avg_pm2.5, ref_dataset)

pm_weighted_col_end <- "pm2020_weighted"

fig_a1_plt_part20 <- gadm2_aqli_2020 %>%
  filter(!is.na(population)) %>%
  mutate(country = "foo") %>%
  group_by(country) %>%
  mutate(
    pop_weights = population / sum(population, na.rm = TRUE),
    across(starts_with("pm"), ~ .x * pop_weights, .names = "{col}_weighted")
  ) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(
    cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end),
    names_to = "years",
    values_to = "pop_weighted_avg_pm2.5"
  ) %>%
  mutate(
    years = as.integer(str_extract(years, "\\d+")),
    region = "National Average",
    ref_dataset = "2020 dataset"
  ) %>%
  select(years, region, pop_weighted_avg_pm2.5, ref_dataset)

pm_weighted_col_end <- "pm2016_weighted"

fig_a1_plt_part16 <- gadm2_aqli_2016 %>%
  filter(!is.na(population)) %>%
  mutate(country = "foo") %>%
  group_by(country) %>%
  mutate(
    pop_weights = population / sum(population, na.rm = TRUE),
    across(starts_with("pm"), ~ .x * pop_weights, .names = "{col}_weighted")
  ) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(
    cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end),
    names_to = "years",
    values_to = "pop_weighted_avg_pm2.5"
  ) %>%
  mutate(
    years = as.integer(str_extract(years, "\\d+")),
    region = "National Average",
    ref_dataset = "2016 dataset"
  ) %>%
  select(years, region, pop_weighted_avg_pm2.5, ref_dataset)

# ------------------------------------------------------------------------------
# Combine all datasets into a single time-series dataset
# ------------------------------------------------------------------------------

final_fig_a1_dataset <- rbind(
  fig_a1_plt_part24,
  fig_a1_plt_part23,
  fig_a1_plt_part22,
  fig_a1_plt_part20,
  fig_a1_plt_part16
)

# ------------------------------------------------------------------------------
# Visualization: Trend comparison across datasets
# ------------------------------------------------------------------------------

ar_appendix_figa.1 <- final_fig_a1_dataset %>%
  ggplot(aes(x = years, y = pop_weighted_avg_pm2.5, color = ref_dataset)) +
  geom_line(lwd = 1.5) +
  
  # X-axis: full time range
  scale_x_continuous(
    breaks = seq(1998, 2024, 2),
    limits = c(1998, 2024)
  ) +
  
  # Y-axis: PM2.5 concentration range
  scale_y_continuous(
    breaks = seq(0, 40, 5),
    limits = c(0, 40)
  ) +
  
  themes_aqli_base +
  scale_color_viridis_d() +
  
  # Line types to distinguish older vs newer datasets
  scale_linetype_manual(values = c(
    "2016 dataset" = "dashed",
    "2020 dataset" = "dashed",
    "2022 dataset" = "dashed",
    "2023 dataset" = "solid"
  )) +
  
  labs(
    x = "Year",
    y = expression("Annual Average " ~ PM[2.5] ~ " Concentration (in µg/m³)"),
    color = ""
  ) +
  
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "#222222"),
    legend.box.background = element_rect(color = "black"),
    axis.text = element_text(size = 20, color = "#222222"),
    axis.title.y = element_text(size = 20, margin = ggplot2::margin(r = 0.6, unit = "cm")),
    axis.title.x = element_text(size = 20, margin = ggplot2::margin(t = 0.6, b = 0.6, unit = "cm")),
    axis.line = element_line(),
    plot.margin = ggplot2::margin(20, 120, 20, 20),
    plot.background = element_rect(color = "white", fill = "white"),
    axis.ticks = element_blank()
  )


