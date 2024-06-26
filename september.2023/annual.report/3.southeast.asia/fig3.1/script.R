# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")

# southeast asia definition
se_asia_def <-  c("Brunei", "Myanmar", "Cambodia", "Timor-Leste", "Indonesia", 
                  "Laos", "Malaysia", "Philippines", "Singapore", "Thailand", 
                  "Vietnam")

# figure 3.1 ------------

# southeast asia figure 3.1 dataset
ar_se_asia_fig3.1_data <- gadm2_aqli_2021 %>%
  filter(country %in% se_asia_def) %>%
  left_join(gadm2_aqli_2021_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2021") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# southeast asia figure 3.1: map
ar_se_asia_fig3.1 <- ar_se_asia_fs_fig3.1_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = gadm1_aqli_2021 %>% filter(name0 %in% se_asia_countries), color = "azure4", fill = "transparent", lwd = 0.1) +
  geom_sf(data = gadm0_aqli_2021 %>% filter(name0 %in% se_asia_countries), color = "cornsilk4", fill = "transparent", lwd = 0.3) +
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
  labs(fill = "Potential gain in life expectancy (Years)") + 
  theme(legend.position = "bottom", 
        legend.justification = c(0.5, 3), 
        legend.background = element_rect(color = "black"), 
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15), 
        plot.title = element_text(hjust = 0.5, size = 15), 
        legend.box.margin = margin(b = 1, unit = "cm"),
        plot.subtitle = element_text(hjust = 0.5, size = 7), 
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"), 
        legend.key = element_rect(color = "black"), 
        legend.box.spacing = unit(0, "cm"), 
        legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1))

