# read in the helper file
source("R/july_2024_helper_script.R")

# Fig 1:  Potential gain in Life Expectancy from permanently reducing PM2.5 ------ 
# from 2022 levels to the WHO PM2.5 guideline.
# colombia fs fig 1 data
colombia_fs_fig1_dataset <- gadm2_aqli_2022 %>%
  mutate(llpp_who_2022 = if_else(pm2022 <= whostandard, 0, llpp_who_2022)) %>%
  filter(country == "Colombia", !is.na(llpp_who_2022)) %>%
  left_join(gadm2_aqli_2022_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  select('country', 'name_1', 'name_2', 'population', 'pm2022', 'llpp_who_2022', 'geometry') %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2022") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# colombia fs figure 1
colombia_fs_fig1 <- colombia_fs_fig1_dataset %>%
  ggplot() +
  geom_sf(mapping = aes(fill = reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = gadm1_aqli_2022_shp %>% filter(name0 == "Colombia"), color = "azure4", fill = "transparent", lwd = 0.15) +
  geom_sf(data = gadm0_aqli_2022_shp %>% filter(name0 == "Colombia"), color = "cornsilk4", fill = "transparent", lwd = 0.5) +
  geom_sf(data = gadm1_aqli_2022_shp %>% filter(name0 == "Colombia", name1 %in% c("Amazonas", "Guaviare", "Vaupés")), 
          color = "black", fill = "transparent", lwd = 0.5) +
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
  labs(fill = "Potential gain in life expectancy (Years)") + 
  theme(legend.position = "bottom", 
        legend.justification = "center", 
        legend.background = element_rect(color = "black"), 
        legend.text = element_text(size = 20, colour = "#222222"),
        legend.title = element_text(size = 20, colour = "#222222"),
        legend.box.margin = margin(b = 1, unit = "cm"), 
        legend.key = element_rect(color = "black"), 
        legend.box.spacing = unit(0, "cm"), 
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5, size = 15), 
        plot.subtitle = element_text(hjust = 0.5, size = 7), 
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic")) +
  guides(fill = guide_legend(nrow = 1))


