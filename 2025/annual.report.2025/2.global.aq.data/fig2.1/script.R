# read in the helper file
source("~/R/july.2025.helper.script.R")

# =========================
# 1. Read & Clean Monitor Data
# =========================

# Read monitor data and retain only non-missing country names
monitoring_data <- read_excel("no_of_monitors_govt_other.xlsx") %>% 
  filter(!is.na(name)) %>%
  mutate(Monitor_govt_pvt = ifelse(ismonitor == TRUE, "govt", "pvt"))

# Filter government monitors and fix inconsistent country names
monitoring_data_gvt <- monitoring_data %>% 
  filter(Monitor_govt_pvt == "govt") %>%
  mutate(name = case_when(
    name == "C√¥te d'Ivoire" ~ "Côte d'Ivoire",
    name == "Bosnia and Herz." ~ "Bosnia and Herzegovina",
    name == "Mexico" ~ "México",
    name == "Dem. Rep. Congo" ~ "Democratic Republic of the Congo",
    name == "United States of America" ~ "United States",
    TRUE ~ name
  ))

# =========================
# 2. Read & Clean Population Data
# =========================
# 
# population_country <- read_excel("aqli_gadm0_2023.xlsx") %>% 
#   select(continent, country, population) %>%
#   mutate(country = case_when(
#     country == "CÃ´te d'Ivoire" ~ "Côte d'Ivoire",
#     country == "SÃ£o TomÃ© and PrÃ­ncipe" ~ "São Tomé and Príncipe",
#     country == "MÃ©xico" ~ "México",
#     country == "United States" ~ "United States",
#     TRUE ~ country
#   ))

# =========================
# 3. Read Region Mapping and Join with Population
# =========================

#aqli_regions <- read_csv("AQLI Regions Countries.csv")

# Keep consistent naming
aqli_regions <- aqli_regions %>%
  mutate(Country = case_when(
    Country == "United States" ~ "United States",
    TRUE ~ Country
  ))

ar_global_fig2.1_data <- gadm2_aqli_2023 %>%
  filter(!is.na(population)) %>%
  mutate(region = case_when(
    country %in% south_asia_def ~ "South Asia",
    country %in% central_and_west_african_countries ~ "Central and west africa",
    country %in% se_asia_vec ~ "South East Asia",
    country %in% latin_america_countries_vec ~ "Latin America",
    country %in% eu_countries ~ "European Union",
    country == "China" ~ "China",
    country %in% mena_countries ~ "Middle East & North Africa",
    TRUE ~ "Rest of the World" # Default case for all others
  )) %>% filter(region!="Rest of the World")


# Filter countries in both region mapping and population data
aqli_region_countries <- population_country %>%
  filter(country %in% aqli_regions$Country) %>%
  left_join(aqli_regions, by = c("country" = "Country")) %>%
  select(`AQLI Region`, country, population)

# =========================
# 4. Join with Monitor Counts
# =========================

# Merge with government monitoring data
final_monitoring_density <- left_join(aqli_region_countries, monitoring_data_gvt, by = c("country" = "name"))

# Replace NA counts with 0 (i.e., countries with no govt monitors)
final_monitoring_density$count[is.na(final_monitoring_density$count)] <- 0

# Keep relevant columns
final_monitoring_density <- final_monitoring_density %>%
  select(`AQLI Region`, country, ismonitor, count, population)

# =========================
# 5. Calculate Monitoring Density
# =========================

final_monitoring_density_summary <- final_monitoring_density %>%
  group_by(`AQLI Region`) %>%
  summarise(
    tot_pop = sum(population, na.rm = TRUE),
    tot_gov_monitor = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(monitor_density = round(tot_gov_monitor * 1000000 / tot_pop, 3))

# Add density buckets for plotting
final_monitoring_density_summary <- final_monitoring_density_summary %>%
  mutate(monitor_density_bucket = case_when(
    monitor_density < 0.1 ~ "< 0.1",
    monitor_density < 0.5 ~ "0.1 < 0.5",
    monitor_density < 1  ~ "0.5 < 1",
    monitor_density < 2.5 ~ "1 < 2.5",
    monitor_density < 5  ~ "2.5 < 5",
    monitor_density < 10    ~ "5 < 10",
    TRUE ~ "≥ 10"
  ))

# =========================
# 6. Plot
# =========================

# Generate bar plot
ar_global_aq_density_fig2.1 <- final_monitoring_density_summary %>%
  ggplot() +
  geom_col(
    aes(x = reorder(`AQLI Region`, monitor_density), y = monitor_density, fill = monitor_density_bucket),
    width = 0.5
  ) +
  geom_hline(
    yintercept = 3,
    linetype = "dashed",
    color = "grey",
    size = 0.5
  ) +
  labs(
    x = "Regions",
    y = "No. of Monitors (per million people)",
    fill = "No. of Monitors (per million people)"
  ) +
  ggthemes::theme_tufte() +
  themes_aqli_base +  # Assuming this is defined earlier
  scale_x_discrete(labels = scales::label_wrap(15)) +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 20, color = "#222222"),
    axis.title.y = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"), color = "#222222"),
    axis.title.x = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color = "#222222"),
    plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 0.8, unit = "cm")),
    plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 0.8, unit = "cm")),
    plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 0.8, unit = "cm")),
    legend.box.background = element_rect(color = "black"),
    plot.background = element_rect(fill = "white", color = "white"),
    axis.ticks.y = element_blank(),
    axis.line = element_line(),
    legend.text = element_text(size = 22, color = "#222222"),
    legend.title = element_text(size = 22, color = "#222222"),
    panel.grid.major.y = element_blank()
  ) +
  scale_y_continuous(breaks = seq(0, 12, 3), limits = c(0, 12)) +
  scale_fill_manual(values = c(
    "< 0.1" = "#FFEDD3",
    "0.1 < 0.5" = "#fed976",
    "0.5 < 1" = "#FFC97A",
    "1 < 2.5" = "#FFA521",
    "2.5 < 5" = "#fc4e2a",
    "5 < 10" = "#fd8d3c",
    "≥ 10" = "#8E2946"
  )) +
  guides(fill = guide_legend(nrow = 1)) +
  geom_text(
    aes(x = 0.7, y = 3),  # New annotation for horizontal line
    label = expression("Recommended minimum monitoring requirement*"),
    size = 6,
    hjust = 0,
    vjust = -0.3,  # Place above the line to avoid overlap with WHO annotation
    check_overlap = TRUE
  )

write_csv(final_monitoring_density_summary, "monitoring_density_chart_bar.csv")

ggsave("ar_global_aq_density_fig2.1.png", ar_global_aq_density_fig2.1, width = 15, height = 10)

ggsave("ar_global_aq_density_fig2.1.svg", ar_global_aq_density_fig2.1, width = 15, height = 10)
