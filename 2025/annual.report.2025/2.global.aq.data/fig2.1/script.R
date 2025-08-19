# read in the helper file
source("~/R/july.2025.helper.script.R").  # Load custom helper functions

# global aq data figure 2.2 ======
# data
monitoring_data <- read_csv("no_of_monitors_govt_other.csv") %>% 
  # remove rows with missing country names
  filter(!is.na(name)) %>%
  mutate(Monitor_govt_pvt = ifelse(ismonitor == TRUE, "govt", "pvt"))

# Filter government monitors and fix inconsistent country names
monitoring_data_gvt <- monitoring_data %>% 
  # keep only ref grade monitor data
  filter(Monitor_govt_pvt == "govt") %>%
  mutate(name = case_when(
    name == "C√¥te d'Ivoire" ~ "Côte d'Ivoire",
    name == "Bosnia and Herz." ~ "Bosnia and Herzegovina",
    name == "Mexico" ~ "México",
    name == "Dem. Rep. Congo" ~ "Democratic Republic of the Congo",
    name == "United States of America" ~ "United States",
    TRUE ~ name
  ))

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

# define colour scheme
monitor_colors <- c( "0 to < 5"   = "#e0feff",
                     "5 to < 10"  = "#b7ebf1",
                     "10 to < 30" = "#66c4d6",
                     "30 to < 50" = "#229dbb",
                     "50 to < 60" = "#00518a")

# define factor levels (legend order)
monitor_levels <- names(monitor_colors)

# figure 2.1 data
ar_global_fig2.1_data <- monitor_density %>%
  mutate(monitor_density_bucket = case_when(is.na(`monitor_density (million)`) ~ NA_character_,
                                            `monitor_density (million)` < 5    ~ "0 to < 5",
                                            `monitor_density (million)` < 10   ~ "5 to < 10",
                                            `monitor_density (million)` < 30   ~ "10 to < 30",
                                            `monitor_density (million)` < 50   ~ "30 to < 50",
                                            `monitor_density (million)` < 60   ~ "50 to < 60")) %>%
  mutate(monitor_density_bucket = factor(monitor_density_bucket, levels = monitor_levels)) %>%
  inner_join(gadm0_aqli_shp, by = c("name" = "name0")) %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# plot
ar_global_fig2.1 <- ggplot() +
  geom_sf(data = ar_global_fig2.1_data, aes(fill = monitor_density_bucket), color = "cornsilk4", lwd = 0.3) +
  geom_sf(data = gadm0_aqli_shp, color = "cornsilk4", fill = "transparent", lwd = 0.3) +
  ggthemes::theme_map() +
  scale_fill_manual(values = monitor_colors,
                    na.value = "grey90", drop = FALSE) +
  labs(fill = "Monitoring Density\n(per million people)") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.key = element_rect(color = "black"),
        legend.box.margin = margin(b = 1, unit = "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.direction = "horizontal") 
