# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")



#> china fs fig1.1 (difference between 2014 and 2021): LYL (experimental figure)-------------------------


# china figure2 dataset
china_fs_fig2_data <- gadm2_aqli_2021 %>%
  filter(country == "China") %>%
  mutate(lyl2021minus2014 = round((pm2021 - pm2014)*0.098, 2)) %>%
  add_aqli_color_scale_buckets(scale_type = "lyldiff", col_name = "lyl2021minus2014") %>%
  left_join(gadm2_aqli_2021_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  select(-geometry, geometry) %>%
  st_as_sf()



# china figure 2
china_fs_fig2 <- china_fs_fig2_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyldiff_bucket, order_lyldiff_bucket)), color = "aliceblue", lwd = 0.05) +
 geom_sf(data = gadm1_aqli_2021_shp %>% filter(name0 == "China"), color = "azure4", fill = "transparent", lwd = 0.15) +
   geom_sf(data = gadm0_aqli_2021_shp %>% filter(name0 == "China"), color = "cornsilk", fill = "transparent", lwd = 0.5) +
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
  labs(fill = "Change in life expectancy between 2014 and 2021 (Years; blue values indicate improvement)", title = "") + 
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


