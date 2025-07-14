# read in the helper file
source("~/R/july.2025.helper.script.R")

# southeast asia definition
se_asia_def <-  c("Brunei", "Myanmar", "Cambodia", "Timor-Leste", "Indonesia",
                  "Laos", "Malaysia", "Philippines", "Singapore", "Thailand",
                  "Vietnam")

# figure 10.1 ------------

# southeast asia figure 10.1 dataset
ar_se_asia_fig10.1_data <- gadm2_aqli_2023 %>%
  filter(country %in% se_asia_def) %>%
  left_join(gadm2_aqli_2023_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2023") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# southeast asia figure 10.1: map
ar_se_asia_fig10.1 <- ar_se_asia_fig10.1_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = gadm1_aqli_2023_shp %>% filter(name0 %in% se_asia_vec), color = "azure4", fill = "transparent", lwd = 0.1) +
  geom_sf(data = gadm0_aqli_2023_shp %>% filter(name0 %in% se_asia_vec), color = "cornsilk4", fill = "transparent", lwd = 0.3) +
  ggthemes::theme_map() +
  scale_fill_manual(values = c("0 to < 0.1" = "#ffffff",
                               "0.1 to < 0.5" = "#fff2e1",
                               "0.5 to < 1" = "#ffedd3",
                               "1 to < 2" = "#ffc97a",
                               "2 to < 3" = "#ffa521",
                               "3 to < 4" = "#eb6c2a",
                               "4 to < 5" =  "#d63333",
                               "5 to < 6" = "#8e2946",
                               ">= 6" = "#451f59"))+
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
        legend.box.spacing = unit(2, "cm"),
        legend.direction = "horizontal",
        plot.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_legend(nrow = 1))