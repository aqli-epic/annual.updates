# read in the helper file
source("R/july_2025_helper_script.R")

# Fig 2: Potential gain in Life Expectancy from permanently reducing PM2.5 ------
# from 2023 levels to the WHO PM2.5 guideline.

## india worldview ------
# india shapefiles
ind_gadm2_shp <- st_read("~/Desktop/AQLI/shapefiles/India_Worldview/gadm2_India_IndWV/india_gadm2.shp")
ind_gadm1_shp <- st_read("~/Desktop/AQLI/shapefiles/India_Worldview/gadm1_India_IndWV/india_state.shp")
ind_gadm0_shp <- st_read("~/Desktop/AQLI/shapefiles/India_Worldview/gadm0_India_IndWV/india_gadm0.shp")

# subset to india WV
india_fs_fig2_data <- gadm2_aqli_2023 %>%
  mutate(llpp_who_2023 = ifelse(pm2023 <= whostandard, 0, llpp_who_2023)) %>%
  inner_join(ind_gadm2_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2022") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# india factsheet figure 1- india
india_fs_fig2 <- india_fs_fig2_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "lightgrey", lwd = 0.05) +
  geom_sf(data = ind_gadm1_shp, color = "azure4", fill = "transparent", lwd = 0.1) +
  geom_sf(data = ind_gadm0_shp, color = "cornsilk4", fill = "transparent", lwd = 0.3) +
  # geom_sf(data = india_state, color = "black", fill = "transparent", lwd = 0.5) +
  ggthemes::theme_map() +
  scale_fill_manual(values = c("0 to < 0.1" = "#FFFFFF",
                               "0.1 to < 0.5" = "#FFF2E1",
                               "0.5 to < 1" = "#FFEDD3",
                               "1 to < 2" = "#FFC97A",
                               "2 to < 3" = "#FFA521",
                               "3 to < 4" = "#EB6C2A",
                               "4 to < 5" = "#D63333",
                               "5 to < 6" = "#8E2946",
                               ">= 6" = "#451F59")) +
  ggthemes::theme_map() +
  labs(fill = "Potential gain in life expectancy (Years) ", title = "") +
  theme(legend.position = "bottom",
        legend.justification = "center",
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

