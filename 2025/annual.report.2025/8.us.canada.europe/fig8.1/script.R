# read in the helper file
source("R/july.2025.helper.script.R")

# US, Canada, Europe figure 8.1 ============
# US + Canada county level shape file
county_shp <- gadm2_aqli_2023_shp %>%
  filter(name0 %in% c("United States", "Canada"))

# Change in life expectancy map (change geo_name to name_2) and remove any counties that belong to Hawaii or Alaska
county_shp <- inner_join(county_shp, gadm2_aqli_2023, by = c("obidgadm2"="objectid_gadm2")) %>%
  filter(!(name1 %in% c("Alaska", "Hawaii")))

# fig 8.1 data
us_can_1998_2023_map_data <- county_shp %>%
  mutate(lyl1998minus2023 = round((pm1998 - pm2023)*0.098, 2)) %>%
  add_aqli_color_scale_buckets("lyldiff", "lyl1998minus2023") %>%
  select(-geometry, geometry)

# plot
ar_us_can_fig8.1 <- us_can_1998_2023_map_data %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyldiff_bucket, order_lyldiff_bucket)),
          color = "aliceblue", lwd = 0.05) +
  geom_sf(data = gadm1_aqli_2023_shp %>% filter(name0 %in% c("United States", "Canada"),
                                           name1 %notin% c("Alaska", "Hawaii")),
          color = "azure4", fill = "transparent", lwd = 0.5) +
  ggthemes::theme_map() +
  scale_fill_manual(values = c(">= 2" = "#008fbb",
                               "0.5 to (< 2)" = "#4fb6d3",
                               "0.1 to (< 0.5)" = "#99dbe9",
                               "0 to (< 0.1)" = "#d2eef4",
                               "-0.1 to (< 0)" = "#ffd393",
                               "-0.5 to (< -0.1)" = "#fea222",
                               "-2 to (< -0.5)" = "#ec6f29",
                               "< -2" = "#d63333")) +
  ggthemes::theme_map() +
  labs(fill = "Change in life expectancy between 1998 and 2023 \n(Years; blue values indicate improvement)", title = "") +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 15),
        # legend.key = element_rect(color = "black"),
        legend.box.margin = margin(b = 1, unit = "cm"),
        plot.subtitle = element_text(hjust = 0.5, size = 7),
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
        legend.key = element_rect(color = "black"),
        legend.box.spacing = unit(0, "cm"),
        legend.direction = "horizontal",
        plot.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_legend(nrow = 1))
