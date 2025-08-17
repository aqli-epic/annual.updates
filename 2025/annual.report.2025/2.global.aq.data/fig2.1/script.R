# -------------------------------------------------------------------------
# Step 1: Load helper script and set working directory
# -------------------------------------------------------------------------
source("~/R/july.2025.helper.script.R").  # Load custom helper functions

# -------------------------------------------------------------------------
# Step 2: Define regional country groups
# -------------------------------------------------------------------------
us_canada <- c("United States", "Canada")

europe_countries <- c(
  "Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium",
  "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark",
  "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland",
  "Ireland", "Italy", "Kazakhstan", "Kosovo", "Latvia", "Liechtenstein", "Lithuania",
  "Luxembourg", "Macedonia", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands",
  "Northern Cyprus", "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino",
  "Serbia", "Slovakia", "Slovenia", "Spain", "Svalbard and Jan Mayen", "Sweden",
  "Switzerland", "Turkey", "Ukraine", "United Kingdom", "Vatican City"
)

# -------------------------------------------------------------------------
# Step 3: Read and clean monitor data
# -------------------------------------------------------------------------
monitoring_data <- read_excel("no_of_monitors_govt_other.xlsx") %>%
  filter(!is.na(name)) %>%                                    # Remove rows with missing country names
  mutate(Monitor_govt_pvt = ifelse(ismonitor == TRUE, "govt", "pvt"))  # Label govt vs. private monitors

# Keep only government monitors and fix inconsistent country naming
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

# Exclude countries without reliable government monitoring data
countries_filter <- c("Afghanistan", "Chad", "Côte d'Ivoire", "Democratic Republic of the Congo",
                      "Gabon", "Guinea", "Iraq", "Mali", "Palestine", "Sudan", "Turkmenistan")

monitoring_data_gvt <- monitoring_data_gvt %>% filter(!name %in% countries_filter)

# -------------------------------------------------------------------------
# Step 4: Read and clean population data
# -------------------------------------------------------------------------
countries_population <- gadm0_aqli_2023 %>%
  select(continent, country, population) %>%
  mutate(country = case_when(
    country == "CÃ´te d'Ivoire" ~ "Côte d'Ivoire",
    country == "SÃ£o TomÃ© and PrÃ­ncipe" ~ "São Tomé and Príncipe",
    country == "MÃ©xico" ~ "México",
    country == "United States" ~ "United States",
    TRUE ~ country
  ))

# -------------------------------------------------------------------------
# Step 5: Map countries to AQLI-defined regions
# -------------------------------------------------------------------------
aqli_region_countries <- countries_population %>%
  mutate(aqli_region = case_when(
    country %in% south_asia_def             ~ "South Asia",
    country %in% central_african_countries  ~ "Central and West Africa",
    country %in% west_african_countries     ~ "Central and West Africa",
    country %in% mid_east_countries         ~ "Middle East and North Africa",
    country %in% north_africa_countries     ~ "North Africa",
    country %in% se_asia_vec                ~ "South East Asia",
    country %in% latin_america_countries_vec~ "Latin America",
    country %in% europe_countries           ~ "Europe",
    country %in% oceania                    ~ "Oceania",
    country %in% us_canada                  ~ "US + Canada"
  )) %>%
  filter(!is.na(aqli_region)) %>%     # Keep only mapped countries
  select(aqli_region, country, population)

# -------------------------------------------------------------------------
# Step 6: Merge monitor and population data
# -------------------------------------------------------------------------
final_monitoring_density <- left_join(aqli_region_countries, monitoring_data_gvt,
                                      by = c("country" = "name"))

# Replace missing monitor counts with 0 (no government monitors)
final_monitoring_density$count[is.na(final_monitoring_density$count)] <- 0

# Keep relevant columns only
final_monitoring_density <- final_monitoring_density %>%
  select(aqli_region, country, ismonitor, count, population)

# -------------------------------------------------------------------------
# Step 7: Calculate monitoring density (per million people)
# -------------------------------------------------------------------------
final_data <- left_join(countries_population, monitoring_data_gvt, by = c("country" = "name"))

final_moni_data_per_m <- final_data %>%
  mutate(`monitor_density (million)` = round(count * 1000000 / population, 3)) %>%
  rename(tot_gov_monitor = count) %>%
  select(continent, country, population, tot_gov_monitor, `monitor_density (million)`)

# -------------------------------------------------------------------------
# Step 8: Prepare GIS plot of monitoring density
# -------------------------------------------------------------------------
# Define color palette for monitor density buckets
monitor_colors <- c(
  "0 to < 5"   = "#e0feff",
  "5 to < 10"  = "#b7ebf1",
  "10 to < 30" = "#66c4d6",
  "30 to < 50" = "#229dbb",
  "50 to < 60" = "#00518a"
)

# Define factor levels (legend order)
monitor_levels <- names(monitor_colors)

# Assign density buckets
ar_global_fig2.2_data <- final_moni_data_per_m %>%
  mutate(monitor_density_bucket = case_when(
    is.na(`monitor_density (million)`) ~ NA_character_,
    `monitor_density (million)` < 5    ~ "0 to < 5",
    `monitor_density (million)` < 10   ~ "5 to < 10",
    `monitor_density (million)` < 30   ~ "10 to < 30",
    `monitor_density (million)` < 50   ~ "30 to < 50",
    `monitor_density (million)` < 60   ~ "50 to < 60"
  )) %>%
  mutate(monitor_density_bucket = factor(monitor_density_bucket, levels = monitor_levels))

# Merge with world shapefile for GIS plotting
ar_global_fig2.2_data <- gadm0_aqli_2023_shp %>%
  left_join(ar_global_fig2.2_data, by = c("name0" = "country"))

# -------------------------------------------------------------------------
# Step 9: Create world map of monitoring density
# -------------------------------------------------------------------------
ar_global_fig2.2 <- ar_global_fig2.2_data %>%
  ggplot() +
  geom_sf(aes(fill = monitor_density_bucket), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = gadm0_aqli_2023_shp, color = "cornsilk4", fill = "transparent", lwd = 0.3) +
  ggthemes::theme_map() +
  scale_fill_manual(values = monitor_colors,
                    na.value = "grey90", drop = FALSE) +
  labs(fill = "Monitoring Density\n(per million people)") +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 3),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15),
    legend.key = element_rect(color = "black"),
    legend.box.margin = margin(b = 1, unit = "cm"),
    legend.box.spacing = unit(2, "cm"),
    legend.direction = "horizontal",
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.7, size = 9, face = "italic")
  ) +
  guides(fill = guide_legend(nrow = 1))


