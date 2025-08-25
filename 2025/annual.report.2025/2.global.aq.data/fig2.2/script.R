# read in the helper file
source("~/R/july.2025.helper.script.R")


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


# Filter countries in both region mapping and population data
# keep Population and assign AQLI regions
aqli_region_countries <- gadm0_aqli_2023 %>%
  select(name, population) %>%
  mutate(`AQLI Region` = case_when(
    name %in% south_asia_def ~ "South Asia",
    name %in% central_and_west_african_countries ~ "Central and west africa",
    name %in% se_asia_vec ~ "South East Asia",
    name %in% latin_america_countries_vec ~ "Latin America",
    name %in% mena_countries ~ "Middle East and North Africa",
    name %in% unlist(european_countries) ~ "Europe",
    name %in% oceania ~ "Oceania",
    name %in% c("United States", "Canada") ~ "US + Canada",
    TRUE ~ "Rest of the World" # Default case for all others
  )) %>%
  filter(`AQLI Region` != "Rest of the World")


# monitor density
monitor_density <- aqli_region_countries %>%
  left_join(monitoring_data_gvt, by = c("name")) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  select(`AQLI Region`, name, ismonitor, count, population) %>%
  mutate(`monitor_density (million)` = round(count * 1000000 / population, 3))


# Add density buckets for plotting
monitor_density <- monitor_density %>%
  mutate(monitor_density_bucket = case_when(
    `monitor_density (million)` < 0.01 ~ "< 0.01",
    `monitor_density (million)` >= 0.01 & `monitor_density (million)` < 0.1 ~ "0.01–0.1",
    `monitor_density (million)` >= 0.1 & `monitor_density (million)` < 0.5 ~ "0.1–0.5",
    `monitor_density (million)` >= 0.5 & `monitor_density (million)` < 1 ~ "0.5–1",
    `monitor_density (million)` >= 1 & `monitor_density (million)` < 2 ~ "1–2",
    `monitor_density (million)` >= 2 & `monitor_density (million)` < 5 ~ "2–5",
    `monitor_density (million)` >= 5 & `monitor_density (million)` < 11 ~ "5–10",
    TRUE ~ "≥ 10"
  )
  )



# Color palette for buckets
monitor_density_colors <- c(
  "< 0.01"   = "#FFF5EB",  
  "0.01–0.1" = "#FEE6CE",  
  "0.1–0.5"  = "#FDBE85",  
  "0.5–1"    = "#FFA521",  
  "1–2"      = "#F16913", 
  "2–5"      = "#D94801",  
  "5–10"     = "#f5781d",  
  "≥ 10"     = "#A63603"   
)

# Generate bar plot
plot_density_moni_aqli_region <- final_monitoring_density_summary %>%
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
    axis.title.y = element_text(size = 20, margin = margin(r = 0.6, unit = "cm"), color = "#222222"),
    axis.title.x = element_text(size = 20, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color = "#222222"),
    plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 0.8, unit = "cm")),
    plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 0.8, unit = "cm")),
    plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 0.8, unit = "cm")),
    legend.box.background = element_rect(color = "black"),
    plot.background = element_rect(fill = "white", color = "white"),
    axis.ticks.y = element_blank(),
    axis.line = element_line(),
    legend.text = element_text(size = 20, color = "#222222"),
    legend.title = element_text(size = 20, color = "#222222"),
    panel.grid.major.y = element_blank()
  ) +
  scale_y_continuous(breaks = seq(0, 12, 3), limits = c(0, 12)) +
  scale_fill_manual(values = monitor_density_colors) +
  guides(fill = guide_legend(nrow = 1)) +
  geom_text(
    aes(x = 0.5, y = 3),  # New annotation for horizontal line
    label = expression("Recommended minimum monitoring requirement*"),
    size = 6,
    hjust = 0,
    vjust = -0.3,  
    check_overlap = TRUE
  )


