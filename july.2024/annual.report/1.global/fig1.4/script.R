# read in the helper file
source("R/july.2024.helper.script.R")

# AQ standards data
ar_global_fig1.4_data <- gadm0_aqli_2022 %>%
  left_join(nat_standard, by = c("country"="Country")) %>%
  select(iso_alpha3, country, natstandard.y, pm2022, llpp_who_2022) %>%
  mutate(aq_std_bucket = if_else(natstandard.y <= 5, "WHO Guideline: 5 µg/m³", NA),
         aq_std_bucket = if_else(natstandard.y > 5 & natstandard.y <= 10, "WHO Interim Target 4: 10 µg/m³", aq_std_bucket),
         aq_std_bucket = if_else(natstandard.y > 10 & natstandard.y <= 15, "WHO Interim Target 3: 15 µg/m³", aq_std_bucket),
         aq_std_bucket = if_else(natstandard.y > 15 & natstandard.y <= 25, "WHO Interim Target 2: 25 µg/m³", aq_std_bucket),
         aq_std_bucket = if_else(natstandard.y > 25 & natstandard.y <= 35, "WHO Interim Target 1: 35 µg/m³", aq_std_bucket),
         aq_std_bucket = if_else(natstandard.y > 35, " > 35 µg/m³", aq_std_bucket)) %>%
  replace_na(list(aq_std_bucket = "No PM2.5 Standard")) %>%
  left_join(gadm0_aqli_2022_shp, by = c("iso_alpha3" = "isoalp3")) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2022") %>%
  select(-geometry, geometry, ) %>%
  st_as_sf()

ar_global_fig1.4_data$aq_std_bucket <- factor(ar_global_fig1.4_data$aq_std_bucket, 
                                              levels=c("WHO Guideline: 5 µg/m³", "WHO Interim Target 2: 25 µg/m³", 
                                                       " > 35 µg/m³", "WHO Interim Target 4: 10 µg/m³", 
                                                       "WHO Interim Target 1: 35 µg/m³", "No PM2.5 Standard", "WHO Interim Target 3: 15 µg/m³" ))

# global fig 1.4
ar_global_fig1.4 <- ggplot() +
  geom_sf(data = gadm0_aqli_2022_shp, color = "cornsilk4", fill = "white", lwd = 0.05) +
  geom_sf(data = ar_global_fig1.4_data, mapping = aes(fill = aq_std_bucket), color = "cornsilk4", lwd = 0.05) +
  ggthemes::theme_map() +
  labs(fill="Air quality standards around the world") +
  scale_fill_manual(values = c("WHO Guideline: 5 µg/m³" = "#5f7aa5", 
                               "WHO Interim Target 4: 10 µg/m³" = "#7197be", 
                               "WHO Interim Target 3: 15 µg/m³" = "#4575b4",
                               "WHO Interim Target 2: 25 µg/m³" = "#74add1", 
                               "WHO Interim Target 1: 35 µg/m³" = "#abd9e9",
                               " > 35 µg/m³" = "#e0f3f8",
                               "No PM2.5 Standard" = "lightgrey" )) +
  theme(plot.title = element_text(hjust = 0.5, size = 15), 
        plot.background = element_rect(fill = "white", color = "white"),
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"), 
        legend.position = "bottom", 
        legend.justification = "center", 
        legend.background = element_rect(color = "black"), 
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        legend.key = element_rect(color = "black"),
        legend.box.margin = margin(b = 1, unit = "cm"),
        legend.box.spacing = unit(0, "cm"), 
        legend.direction = "horizontal") +
  guides(fill = guide_legend(ncol = 3, nrow = 3))
