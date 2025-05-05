# read in the helper file
source("~/R/july.2025.helper.script.R")

# Fig 1:  Potential gain in Life Expectancy from permanently reducing PM2.5 ------
# from 2023 levels to the WHO PM2.5 guideline.
# nepal fs fig 1 data
nepal_fs_fig1_dataset <- gadm2_aqli_2023 %>%
  filter(country == "Nepal") %>%
  left_join(gadm2_aqli_2023_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  select('country', 'name_1', 'name_2', 'population', 'pm2023', 'llpp_who_2023', 'geometry') %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2023") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# nepal fs figure 1
nepal_fs_fig1 <- nepal_fs_fig1_dataset %>%
  ggplot() +
  geom_sf(mapping = aes(fill = reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = gadm1_aqli_2023_shp %>% filter(name0 == "Nepal"), color = "azure4", fill = "transparent", lwd = 0.15) +
  geom_sf(data = gadm0_aqli_2023_shp %>% filter(name0 == "Nepal"), color = "cornsilk4", fill = "transparent", lwd = 0.5) +
  ggthemes::theme_map() +
  scale_fill_manual(values = c(
    "0 to < 0.1" = "#FFFFFF", 
    "0.1 to < 0.5" = "#FFF2E1", 
    "0.5 to < 1" = "#FFEDD3", 
    "1 to < 2" = "#FFC97A", 
    "2 to < 3" = "#FFA521", 
    "3 to < 4" = "#EB6C2A", 
    "4 to < 5" = "#D63333", 
    "5 to < 6" = "#8E2946", 
    ">= 6" = "#451F59")) +
  ggthemes::theme_map() +
  labs(fill = "Potential gain in life expectancy (Years)") +
  theme(legend.position = "bottom",
        legend.justification = c(0.5, 3),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 20, color="#222222"),
        legend.title = element_text(size = 20, color="#222222"),
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.box.margin = margin(b = 1, unit = "cm"),
        plot.subtitle = element_text(hjust = 0.5, size = 7),
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
        legend.key = element_rect(color = "black"),
        legend.box.spacing = unit(2, "cm"),
        legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1))