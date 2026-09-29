# ------------------------------------------------------------------------------
# AQLI 2026 – China Regional PM2.5 Trends
# Description : Computes population-weighted PM2.5 trends for major regions 
#               (BTH, YRD, PRD) and national average for China (1998–2024).
#
# Developed By: Purushottam Gupta
# Role        : Data Architect/Data Analyst, AQLI (University of Chicago)
# Contact     : guptap@uchicago.edu
# Dependencies: dplyr, tidyr, ggplot2, stringr, ggrepel, ggthemes
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Load Dependencies & Source Files
# ------------------------------------------------------------------------------
source("~/R/helper_function_2026.R")

# ------------------------------------------------------------------------------
# Data Preparation: Base China Dataset
# ------------------------------------------------------------------------------
china_base_data <- gadm2_aqli_2024 %>%
  filter(country == "China", !is.na(population))

bth_region <- c("Beijing", "Tianjin", "Hebei")
yrd_region <- c("Shanghai", "Jiangsu", "Zhejiang")

prd_region_name_2 <- c(
  "Dongguan", "Foshan", "Guangzhou", "Huizhou", "Jiangmen",
  "Shenzhen", "Zhaoqing", "Zhongshan", "Zhuhai"
)

prd_region_name_1 <- c("Hong Kong", "Macau")

# ------------------------------------------------------------------------------
# Assign Regions
# ------------------------------------------------------------------------------
china_base_data <- china_base_data %>%
  mutate(
    region = "Others",
    region = ifelse(name_1 %in% bth_region, "BTH", region),
    region = ifelse(name_1 %in% yrd_region, "YRD", region),
    region = ifelse(name_1 %in% prd_region_name_1, "PRD", region),
    region = ifelse(name_2 %in% prd_region_name_2, "PRD", region)
  )

# ------------------------------------------------------------------------------
# Regional Trends (Population-weighted)
# ------------------------------------------------------------------------------
china_region_trends <- china_base_data %>%
  group_by(region) %>%
  mutate(
    pop_weights = population / sum(population, na.rm = TRUE)
  ) %>%
  mutate(
    across(starts_with("pm"), ~ .x * pop_weights, .names = "{col}_weighted")
  ) %>%
  summarise(across(ends_with("weighted"), sum), .groups = "drop") %>%
  pivot_longer(
    cols = starts_with("pm"),
    names_to = "years",
    values_to = "pm25"
  ) %>%
  mutate(
    years = as.integer(stringr::str_extract(years, "\\d+"))
  ) %>%
  filter(region %in% c("YRD", "BTH", "PRD"))

# ------------------------------------------------------------------------------
# National Trend
# ------------------------------------------------------------------------------
china_national_trend <- china_base_data %>%
  mutate(
    pop_weights = population / sum(population, na.rm = TRUE)
  ) %>%
  mutate(
    across(starts_with("pm"), ~ .x * pop_weights, .names = "{col}_weighted")
  ) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(
    cols = starts_with("pm"),
    names_to = "years",
    values_to = "pm25"
  ) %>%
  mutate(
    years = as.integer(stringr::str_extract(years, "\\d+")),
    region = "China"
  )

# ------------------------------------------------------------------------------
# Combine Dataset
# ------------------------------------------------------------------------------
china_trend_dataset <- bind_rows(
  china_region_trends,
  china_national_trend
) %>%
  select(years, region, pm25)

china_trend_dataset$region <- factor(
  china_trend_dataset$region,
  levels = c("BTH", "China", "YRD", "PRD")
)

# ------------------------------------------------------------------------------
# Label Positions
# ------------------------------------------------------------------------------
last_points <- china_trend_dataset %>%
  group_by(region) %>%
  slice_max(years, n = 1) %>%
  ungroup() %>%
  mutate(label_x = years + 0.4, label_y = pm25)

# ------------------------------------------------------------------------------
# Visualization: Regional PM2.5 Trends
# ------------------------------------------------------------------------------
ar_chinal_fig8.3 <- china_trend_dataset %>% ggplot() +
  geom_line(
    aes(
      x = years,
      y = pm25,
      color = region,
      linetype = region
    ),
    linewidth = 1.1
  ) +
  geom_hline(yintercept = 5, linewidth = 0.5, linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = 35, linewidth = 0.5, linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = 30, linewidth = 0.5, linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = 2014, linewidth = 0.5, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(1998, 2024, 2)) +
  scale_color_manual(
    values = c(
      "PRD" = "#4E79A7",
      "China" = "#1F4E8C",
      "BTH" = "#2C7FB8",
      "YRD" = "#2C7FB8"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "PRD" = "dashed",
      "China" = "solid",
      "BTH" = "dashed",
      "YRD" = "dashed"
    )
  ) +
  ggthemes::theme_clean() +
  themes_aqli_base +
  labs(
    x = "Year",
    y = expression("Annual Average " ~ PM[2.5] ~ " Concentration (µg/m³)"),
    title = "",
    subtitle = ""
  ) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 20, color = "#222222"),
    axis.title.y = element_text(size = 20, margin = margin(r = 0.6, unit = "cm")),
    axis.title.x = element_text(size = 20, margin = margin(t = 0.6, b = 0.6, unit = "cm")),
    plot.margin = margin(20, 120, 20, 20),
    axis.line = element_line(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white", color = "white")
  ) +
  annotate("text", x = 1999.5, y = 7.1,
           hjust = 0, size = 5, color = "#333333",
           label = expression("WHO" ~ PM[2.5] ~ " Guideline (2021): 5 µg/m³")) +
  annotate("text", x = 2016, y = 92.8,
           size = 5, color = "#333333",
           label = stringr::str_wrap("China announces war on pollution", 16)) +
  annotate("text", x = 1999.5, y = 36.7,
           hjust = 0, size = 5, color = "#333333",
           label = expression("China Old National" ~ PM[2.5] ~ " Standard: 35 µg/m³")) +
  annotate("text", x = 1999.5, y = 31,
           hjust = 0, size = 5, color = "#333333",
           label = expression("China Revised National" ~ PM[2.5] ~ " Standard: 30 µg/m³")) +
  ggrepel::geom_text_repel(
    data = last_points,
    aes(
      x = label_x,
      y = label_y,
      label = region,
      color = region,
      fontface = ifelse(region == "China", "bold", "plain")
    ),
    hjust = 0,
    size = 4,
    segment.color = "grey50",
    segment.size = 0.5,
    direction = "y",
    nudge_x = 0.6,
    box.padding = 0,
    point.padding = 0,
    min.segment.length = 0,
    show.legend = FALSE
  )