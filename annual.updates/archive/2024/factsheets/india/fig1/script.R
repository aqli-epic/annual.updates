# read in the helper file
source("R/july_2024_helper_script.R")

# Fig 1: Potential gain in Life Expectancy from permanently reducing PM2.5 ------ 
# from 2022 levels to the WHO PM2.5 guideline.

# Generate India gadm0 and gadm2 shapefiles
india_gadm0 <- st_read("~/Desktop/AQLI/shapefiles/India_country_IndWV/india_gadm0.shp")

# aksai chin
aksai_chin_gadm1 <- st_read("~/Desktop/AQLI/shapefiles/aksai_chin/aksai_chin.shp")

aksai_chin_gadm2 <- aksai_chin_gadm1  %>%
  mutate(obidgadm2 = if_else(name1 == "Xizang", 11186, 11177),
         name0 = "India",
         name1 = "Jammu and Kashmir",
         name2 = "Aksai Chin") %>%
  select(obidgadm2, name0, name1, name2, geometry)

# azad kashmir and gilgit baltistan  
azad_kashmir_gilgit_baltistan <- gadm2_aqli_2022_shp %>%
  filter(name0 == "Pakistan", name1 %in% c("Azad Kashmir", "Gilgit Baltistan")) %>%
  mutate(name0 = "India",
         name1 = "Jammu and Kashmir")

# india wv
india_gadm2 <- gadm2_aqli_2022_shp %>%
  filter(name0 == "India") %>%
  bind_rows(azad_kashmir_gilgit_baltistan) %>%
  bind_rows(aksai_chin_gadm2)

# plot 1 data
india_fs_fig1_data <- gadm2_aqli_2022 %>%
  select(objectid_gadm2, iso_alpha3, country, name_1, name_2, population, pm2022, llpp_who_2022) %>%
  inner_join(india_gadm2, by = c("objectid_gadm2" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2022") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# india factsheet figure 1
india_fs_fig1 <- india_fs_fig1_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "lightgrey", lwd = 0.05) +
  geom_sf(data = india_state, color = "black", fill = "transparent", lwd = 0.5) +
  ggthemes::theme_map() + 
  scale_fill_manual(values = c("0 to < 0.1" = "#ffffff", 
                               "0.1 to < 0.5" = "#ffeda0", 
                               "0.5 to < 1" = "#fed976", 
                               "1 to < 2" = "#feb24c", 
                               "2 to < 3" = "#fd8d3c", 
                               "3 to < 4" = "#fc4e2a", 
                               "4 to < 5" = "#e31a1c", 
                               "5 to < 6" = "#bd0026", 
                               ">= 6" = "#800026")) +
  ggthemes::theme_map() +
  labs(fill = "Potential gain in life expectancy (Years) ", title = "") + 
  theme(legend.position = "bottom", 
        legend.justification = c(0.5, 3), 
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


