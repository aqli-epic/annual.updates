# read in the helper file
source("R/july.2024.helper.script.R")

#Figure 1: Potential gain in life expectancy from permanently reducing PM2.5 from 2022 concentration to the WHO guideline

drc_fs_fig1_dataset <- gadm2_aqli_2022 %>%
  filter(country == "Democratic Republic of the Congo") %>%
  filter(name_2 != "Tshumbe") %>%
  left_join(gadm2_aqli_2022_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  select('country', 'name_1', 'name_2', 'population', 'pm2022', 'llpp_who_2022', 'geometry') %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2022") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# drc fs figure 1
drc_fs_fig1 <- drc_fs_fig1_dataset %>%
  ggplot() +
  geom_sf(mapping = aes(fill = reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = gadm1_aqli_2022_shp %>% filter(name0 == "Democratic Republic of the Congo"), color = "azure4", fill = "transparent", lwd = 0.15) +
  geom_sf(data = gadm0_aqli_2022_shp %>% filter(name0 == "Democratic Republic of the Congo"), color = "cornsilk4", fill = "transparent", lwd = 0.5) +
  ggthemes::theme_map() +
  scale_fill_manual(values = c("0 to < 0.1" = "#ffffff",
                               "0.1 to < 0.5" = "#ffeda0",
                               "0.5 to < 1" = "#fed976",
                               "1 to < 2" = "#feb24c",
                               "2 to < 3" = "#fd8d3c",
                               "3 to < 4" = "#fc4e2a",
                               "4 to < 5" = "#e31a1c",
                               "5 to < 6" = "#bd0026",
                               ">= 6" = "#800026"))+
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

