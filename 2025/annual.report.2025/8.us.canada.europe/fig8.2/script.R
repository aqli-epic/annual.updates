# read in the helper file
source("R/july.2025.helper.script.R")

# US, Canada, Europe figure 8.2 ============
# get USA + Canada country level data from the color file
color_data_us_can <- gadm2_aqli_2023 %>%
  filter(country %in% c("United States", "Canada"), name_1 != "Alaska", name_1 != "Hawaii")

# filter colormap (county level) shape file to only include United States
colormap_shp_us_can <- gadm2_aqli_2023_shp %>%
  filter(name0 %in% c("United States", "Canada"), name1 != "Alaska", name1 != "Hawaii")

# rename columns in the county level shape file
colormap_shp_us_can <- colormap_shp_us_can %>%
  rename(country = name0,
         name_1 = name1,
         name_2 = name2)

# join colormap and county level shape file and adding a grouping column
ar_us_fig8.3_data <- colormap_shp_us_can %>%
  left_join(color_data_us_can, by = c("country", "name_1", "name_2")) %>%
  mutate(pollution_category = ifelse(pm2023 >= 0 & pm2023 <= 5, "0 - 5", pm2023),
         pollution_category = ifelse(pm2023 > 5 & pm2023 <= 10, "> 5 - 10", pollution_category),
         pollution_category = ifelse(pm2023 > 10 & pm2023 <= 15, "> 10 - 15", pollution_category),
         pollution_category = ifelse(pm2023 > 15, "> 15", pollution_category)) %>%
  mutate(order_pollution_category = ifelse(pollution_category == "0 - 5", 1, 0),
         order_pollution_category = ifelse(pollution_category == "> 5 - 10", 2, order_pollution_category),
         order_pollution_category = ifelse(pollution_category == "> 10 - 15", 3, order_pollution_category),
         order_pollution_category = ifelse(pollution_category == "> 15", 4, order_pollution_category))

# plot
ar_us_can_fig8.3 <- ggplot(data = ar_us_fig8.3_data) +
  geom_sf(mapping = aes(fill = fct_reorder(pollution_category, order_pollution_category)), color = "aliceblue") +
  geom_sf(data = gadm1_aqli_2023_shp %>% filter(name0 %in% c("United States", "Canada"), name1 != "Alaska", name1 != "Hawaii"), fill = "transparent", color = "white", lwd = 1) +
  geom_sf(data = gadm1_aqli_2023_shp %>% filter(name0 %in% c("United States", "Canada"), name1 == "California", name1 != "Alaska", name1 != "Hawaii"), fill = "transparent", color = "white", lwd = 1) +
  ggthemes::theme_map() +
  labs(fill = expression("Annual Average" ~ PM[2.5] ~ " Concentration (in  µg/m³)"), title = "") +
  scale_fill_manual(values = c("0 - 5" = "powderblue", "> 5 - 10" = "#FFCC66", "> 10 - 15" = "chocolate2", "> 15" = "darkred")) +
  theme(plot.background = element_rect(colour = "white", fill = "white"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 16),
        legend.box.background = element_rect(color = "black"))
