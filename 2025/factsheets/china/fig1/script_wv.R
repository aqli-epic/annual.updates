# read in the helper file
source("~/R/july.2025.helper.script.R")

# China figure 7.2 ============
# filter data for china and create shapefile
# generate China gadm0, gadm1 and gadm2 shapefiles
arunachal_gadm2 <- gadm2_aqli_2023_shp %>%
  filter(name0 == "India", name1 %in% c("Arunachal Pradesh")) %>%
  mutate(name0 = "China")

taiwan_gadm2 <- gadm2_aqli_2023_shp %>%
  filter(name0 == "Taiwan") %>%
  mutate(name0 = "China")

# china wv
china_wv_gadm2 <- gadm2_aqli_2023_shp %>%
  filter(name0 == "China") %>%
  bind_rows(arunachal_gadm2) %>%
  bind_rows(taiwan_gadm2)

arunachal_gadm1 <- gadm1_aqli_2023_shp %>%
  filter(name0 == "India", name1 %in% c("Arunachal Pradesh")) %>%
  mutate(name0 = "China")

taiwan_gadm1 <- gadm1_aqli_2023_shp %>%
  filter(name0 == "Taiwan") %>%
  mutate(name0 = "China")

# china wv
china_wv_gadm1 <- gadm1_aqli_2023_shp %>%
  filter(name0 == "China") %>%
  bind_rows(arunachal_gadm1) %>%
  bind_rows(taiwan_gadm1)

china_wv_gadm0 <- st_read("~/shapefile/AQLI Shapefiles/China_Worldview/gadm0_China_ChnWV/china_gadm0.shp") %>%
  mutate(name0 = "China",
         obidgadm0 = 46,
         isoalp3 = "CHN") %>%
  select(obidgadm0, isoalp3, name0, geometry)

# china figure 7.2 ===========
# data
ar_china_fig7.2_data <- gadm2_aqli_2023 %>%
  inner_join(china_wv_gadm2, by = c("objectid_gadm2" = "obidgadm2")) %>%
  #filter(country == "China", !is.na(llpp_who_2023)) %>%
  mutate(lyl2014minus2023 = round((pm2014 - pm2023)*0.098, 1)) %>% # to reflect gain in life expectancy
  add_aqli_color_scale_buckets(scale_type = "lyldiff", col_name = "lyl2014minus2023") %>% 
  #left_join(gadm2_aqli_2023_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# plot
ar_china_fig7.2 <- ar_china_fig7.2_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyldiff_bucket, order_lyldiff_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data =  china_wv_gadm1, color = "azure4", fill = "transparent", lwd = 0.15) +
  geom_sf(data =  china_wv_gadm0, color = "cornsilk4", fill = "transparent", lwd = 0.5) +
  ggthemes::theme_map() + 
  scale_fill_manual(values = c(">= 2" = "#008fbb",
                               "0.5 to (< 2)" = "#4fb6d3",
                               "0.1 to (< 0.5)" = "#99dbe9",
                               "0 to (< 0.1)" = "#d2eef4",
                               "-0.1 to (< 0)" = "#ffd393",
                               "-0.5 to (< -0.1)" = "#fea222",
                               "-2 to (< -0.5)" = "#ec6f29",
                               "< -2" = "#d63333")) +
  ggthemes::theme_map() +
  labs(fill = "Change in life expectancy between 2014 and 2023 \n(Years; blue values indicate improvement)", title = "") + 
  theme(legend.position = "bottom", 
        legend.justification = "center", 
        legend.background = element_rect(color = "black"), 
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15), 
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.box.margin = margin(b = 1, unit = "cm"),
        plot.subtitle = element_text(hjust = 0.5, size = 7), 
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
        legend.key = element_rect(color = "black"), 
        legend.box.spacing = unit(0, "cm"), 
        legend.direction = "horizontal",
        plot.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_legend(nrow = 1))