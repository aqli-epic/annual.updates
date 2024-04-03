# read in the helper file
source("R/july.2024.helper.script.R")

# figure 7.2 (US)======================================

# get USA country level data from the color file
color_data_usa <- gadm2_aqli_2021 %>%
  filter(country == "United States", name_1 != "Alaska", name_1 != "Hawaii")

# filter colormap (county level) shape file to only include United States
colormap_shp_usa <- gadm2_aqli_2021_shp %>%
  filter(name0  == "United States", name1 != "Alaska", name1 != "Hawaii")

# rename columns in the USA county level shape file
colormap_shp_usa <- colormap_shp_usa %>%
  rename(country = name0,
         name_1 = name1, 
         name_2 = name2)


# chart5 dataset: join colormap and USA county level shape file and adding a grouping column
ar_fig7.2_data <- colormap_shp_usa %>%
  left_join(color_data_usa, by = c("country", "name_1", "name_2")) %>%
  mutate(pollution_category = ifelse(pm2021 >= 0 & pm2021 <= 5, "0 - 5", pm2021), 
         pollution_category = ifelse(pm2021 > 5 & pm2021 <= 10, "> 5 - 10", pollution_category), 
         pollution_category = ifelse(pm2021 > 10 & pm2021 <= 15, "> 10 - 15", pollution_category), 
         pollution_category = ifelse(pm2021 > 15, "> 15", pollution_category)) %>%
  mutate(order_pollution_category = ifelse(pollution_category == "0 - 5", 1, 0), 
         order_pollution_category = ifelse(pollution_category == "> 5 - 10", 2, order_pollution_category), 
         order_pollution_category = ifelse(pollution_category == "> 10 - 15", 3, order_pollution_category), 
         order_pollution_category = ifelse(pollution_category == "> 15", 4, order_pollution_category))



# chart number 7.2 plot
ar_fig7.2 <- ggplot(data = ar_fig7.2_data) +
  geom_sf(mapping = aes(fill = fct_reorder(pollution_category, order_pollution_category)), color = "aliceblue") + 
  geom_sf(data = gadm1_aqli_2021_shp %>% filter(name0 == "United States", name1 != "Alaska", name1 != "Hawaii"), fill = "transparent", color = "white", lwd = 1) +
   geom_sf(data = gadm1_aqli_2021_shp %>% filter(name0 == "United States", name1 == "California", name1 != "Alaska", name1 != "Hawaii"), fill = "transparent", color = "white", lwd = 1) +
  ggthemes::theme_map() +
  labs(fill = expression("Annual Average" ~ PM[2.5] ~ " Concentration (in  µg/m³)"), title = "") +
  scale_fill_manual(values = c("0 - 5" = "powderblue", "> 5 - 10" = "#FFCC66", "> 10 - 15" = "chocolate2", "> 15" = "darkred")) +
  # ggtitle(expression(paste("US 2020 PM2.5 concentrations, (in ", mu, "g","/", m^3, ")", ""))) +
  theme(plot.background = element_rect(colour = "white", fill = "white"), 
        legend.position = "bottom", 
        legend.justification = "center", 
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 16),
        legend.box.background = element_rect(color = "black")) 


