# read in the helper file
source("R/july.2025.helper.script.R")

# Central and West Africa figure 3.1 ============
# cw africa figure 3.1 dataset
ar_cw_africa_fig3.1_data <- gadm2_aqli_2023 %>%
  filter(country %in% central_and_west_african_countries, !is.na(llpp_who_2023)) %>%
  left_join(gadm2_aqli_2023_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2023") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# cw africa fig1 (burlywood4)
ar_cw_africa_fig3.1 <- ar_cw_africa_fig3.1_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = gadm1_aqli_2023_shp %>% filter(name0 %in% central_and_west_african_countries), color = "azure4", fill = "transparent", lwd = 0.1) +
  geom_sf(data = gadm0_aqli_2023_shp %>% filter(name0 %in% central_and_west_african_countries), color = "cornsilk4", fill = "transparent", lwd = 0.3) +
  ggthemes::theme_map() +
  scale_fill_manual(values = c("0 to < 0.1" = "#929292", 
                               "0.1 to < 0.5" = "#9b8f80", 
                               "0.5 to < 1" = "#9f8f78", 
                               "1 to < 2" = "#ba8a3e", 
                               "2 to < 3" = "#d27f00", 
                               "3 to < 4" = "#ce5410", 
                               "4 to < 5" = "#b90d1f", 
                               "5 to < 6" = "#750c32", 
                               ">= 6" = "#2f0a44")) +
  ggthemes::theme_map() +
  labs(fill = "Potential gain in life expectancy (Years)", 
       title = "", subtitle = "") +
  theme(legend.position = "bottom", 
        legend.justification = "center", 
        legend.background = element_rect(color = "black"), 
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15), 
        plot.title = element_text(hjust = 0.5, size = 15), 
        # legend.key = element_rect(color = "black"),
        legend.box.margin = margin(b = 1, unit = "cm"),
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"), 
        legend.key = element_rect(color = "black"), 
        legend.box.spacing = unit(0, "cm"), 
        legend.direction = "horizontal", 
        plot.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_legend(nrow = 1))
