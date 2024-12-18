# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Chart number 5: US PM2.5 Pollution with a special focus on California (LA Times graph)

# get USA country level data from the color file
color_data_usa <- color_2020 %>%
  filter(country == "United States", name_1 != "Alaska", name_1 != "Hawaii")

# filter colormap (county level) shape file to only include United States
colormap_shp_usa <- colormap_shp %>%
  filter(NAME_0  == "United States", NAME_1 != "Alaska", NAME_1 != "Hawaii")

# read USA state level shape file (borders of this will be overlayed on the county level file)
colormap_shp_usa_state_wise <- st_read("./june.2022/other.important.calculations.data/usa.state.level.shape.file/foo/foo1.shp")

# rename columns in the USA county level shape file
colormap_shp_usa <- colormap_shp_usa %>%
  rename(country = NAME_0,
         name_1 = NAME_1,
         name_2 = NAME_2)


# chart5 dataset: join colormap and USA county level shape file and adding a grouping column
chart5_dataset <- colormap_shp_usa %>%
  left_join(color_data_usa, by = c("country", "name_1", "name_2")) %>%
  mutate(pollution_category = ifelse(pm2020 >= 0 & pm2020 <= 5, "0-5", pm2020),
         pollution_category = ifelse(pm2020 > 5 & pm2020 <= 10, ">5-10", pollution_category),
         pollution_category = ifelse(pm2020 > 10 & pm2020 <= 15, ">10-15", pollution_category),
         pollution_category = ifelse(pm2020 > 15, ">15", pollution_category)) %>%
  mutate(order_pollution_category = ifelse(pollution_category == "0-5", 1, 0),
         order_pollution_category = ifelse(pollution_category == ">5-10", 2, order_pollution_category),
         order_pollution_category = ifelse(pollution_category == ">10-15", 3, order_pollution_category),
         order_pollution_category = ifelse(pollution_category == ">15", 4, order_pollution_category))


# chart number 5 plot
chart5 <- ggplot(data = chart5_dataset) +
  geom_sf(mapping = aes(fill = fct_reorder(pollution_category, order_pollution_category)), color = "aliceblue") +
  geom_sf(data = colormap_shp_usa_state_wise, fill = "transparent", color = "white", lwd = 1) +
  geom_sf(data = colormap_shp_usa_state_wise %>% filter(name_1 == "California"), fill = "transparent", color = "white", lwd = 1) +
  ggthemes::theme_map() +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("0-5" = "powderblue", ">5-10" = "#FFCC66", ">10-15" = "chocolate2", ">15" = "darkred")) +
  ggtitle(expression(paste("US 2020 PM2.5 concentrations, (in ", mu, "g","/", m^3, ")", "")))


