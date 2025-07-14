# read in the helper file
source("R/july.2025.helper.script.R")

# US, Canada figure 3.2 ============
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
ar_us_fig3.2_data <- colormap_shp_us_can %>%
  left_join(color_data_us_can, by = c("country", "name_1", "name_2")) %>%
  add_aqli_color_scale_buckets("pollution", "pm2023")

# plot
ar_us_can_fig3.2 <- ggplot(data = ar_us_fig3.2_data) +
  geom_sf(mapping = aes(fill = fct_reorder(pollution_category, order_pollution_category)), color = "aliceblue") +
  geom_sf(data = gadm1_aqli_2023_shp %>% filter(name0 %in% c("United States", "Canada"), name1 != "Alaska", name1 != "Hawaii"), fill = "transparent", color = "white", lwd = 1) +
  geom_sf(data = gadm1_aqli_2023_shp %>% filter(name0 %in% c("United States", "Canada"), name1 == "California", name1 != "Alaska", name1 != "Hawaii"), fill = "transparent", color = "white", lwd = 1) +
  ggthemes::theme_map() +
  labs(fill = expression("Annual Average" ~ PM[2.5] ~ " Concentration (in  µg/m³)"), title = "") +
  scale_fill_manual(values = c("0 to < 5" = "#e0feff",
                               "5 to < 10" = "#b7ebf1",
                               "10 to < 20" = "#8fd8e4",
                               "20 to < 30" = "#66c4d6",
                               "30 to < 40" = "#3db1c8",
                               "40 to < 50" = "#3f8dac")) +
  theme(plot.background = element_rect(colour = "white", fill = "white"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 16),
        legend.box.background = element_rect(color = "black"))
