# read in the helper file
source("R/july.2024.helper.script.R")

#Figure 1: Change in life expectancy due to change in PM2.5 concentration in 235 counties in the United States between 1970 and 2022.
#Only three counties (in orange) are losing life years due to particulate pollution increasing in 2022 compared to 1970

# percent reduction in pollution since 1970
caa_pct_reduction <- 0.67

#> use as is or source from our folder (or use gadm1_aqli_2022 shape file for the US)
# US state level shapefile
us_states_shp <- gadm1_aqli_2022_shp %>%
  filter(!(name1 %in% c("Alaska", "Hawaii")))


# this is the county level shape file. (Filter this for USA)
county_shp <- gadm2_aqli_2022_shp %>%
  filter(name0 == "United States")


# use county pm25 for aqli csv file
county_data <- read_csv("C:/Users/Aarsh/Desktop/county_pm25_foraqli_stats.csv")

# country level files
country <- read_csv("C:/Users/Aarsh/Downloads/CleanAirAct/aqli_gadm0_2022.csv")


# Change in life expectancy map (change geo_name to name_2) and remove any counties that belong to Hawaii or Alaska
county_shp <- inner_join(county_shp, county_data, by = c("obidgadm2"="objectid_gadm2")) %>%
	filter(!(name2 %in% c("Anchorage", "Honolulu")))



# us figure 1
us_1970_2022_map_data <- county_shp %>%
  mutate(lifeyears_saved_reverse = round((pm2022 - pm1970)*0.098, 2)) %>%
  add_aqli_color_scale_buckets("lyldiff", "lifeyears_saved_reverse") %>%
  select(-geometry, geometry)

us_fs_fig1 <- us_1970_2022_map_data %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyldiff_bucket, order_lyldiff_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = gadm1_aqli_2022_shp %>% filter(name0 == "United States", name1 %notin% c("Alaska", "Hawaii")), color = "azure4", fill = "transparent", lwd = 0.5) +
  ggthemes::theme_map() +
  scale_fill_manual(values = c("< -2" = "#4575b4",
                               "-2 to (< -0.5)" = "#74add1",
                               "-0.5 to (< -0.1)" = "#abd9e9",
                               "-0.1 to (< 0)" = "#e0f3f8",
                               "0 to (< 0.1)" = "#fee090",
                               "0.1 to (< 0.5)" = "#fdae61",
                               "0.5 to (< 2)" = "#f46d43",
                               ">= 2" = "#d73027")) +
  ggthemes::theme_map() +
  labs(fill = "Change in life expectancy between 1970 and 2022 (Years; blue values indicate improvement)", title = "") +
  theme(legend.position = "bottom",
        legend.justification = c(0.5, 3),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 15, color="#222222"),
        legend.title = element_text(size = 15, color="#222222"),
        plot.title = element_text(hjust = 0.5, size = 15),
        # legend.key = element_rect(color = "black"),
        legend.box.margin = margin(b = 1, unit = "cm"),
        plot.subtitle = element_text(hjust = 0.5, size = 7),
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
        legend.key = element_rect(color = "black"),
        legend.box.spacing = unit(2, "cm"),
        legend.direction = "horizontal",
        plot.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_legend(nrow = 1))


