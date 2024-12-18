# read in the helper file
source("R/july.2024.helper.script.R")

# generate China gadm0, gadm1 and gadm2 shapefiles
arunachal_gadm2 <- gadm2_aqli_2022_shp %>%
  filter(name0 == "India", name1 %in% c("Arunachal Pradesh")) %>%
  mutate(name0 = "China")

taiwan_gadm2 <- gadm2_aqli_2022_shp %>%
  filter(name0 == "Taiwan") %>%
  mutate(name0 = "China")

# china wv
china_wv_gadm2 <- gadm2_aqli_2022_shp %>%
  filter(name0 == "China") %>%
  bind_rows(arunachal_gadm2) %>%
  bind_rows(taiwan_gadm2)

arunachal_gadm1 <- gadm1_aqli_2022_shp %>%
  filter(name0 == "India", name1 %in% c("Arunachal Pradesh")) %>%
  mutate(name0 = "China")

taiwan_gadm1 <- gadm1_aqli_2022_shp %>%
  filter(name0 == "Taiwan") %>%
  mutate(name0 = "China")

# china wv
china_wv_gadm1 <- gadm1_aqli_2022_shp %>%
  filter(name0 == "China") %>%
  bind_rows(arunachal_gadm1) %>%
  bind_rows(taiwan_gadm1)

china_gadm0 <- st_read("~/Desktop/AQLI/shapefiles/China_country_ChnWV/china_gadm0.shp") %>%
  mutate(name0 = "China",
         obidgadm0 = 46,
         isoalp3 = "CHN") %>%
  select(obidgadm0, isoalp3, name0, geometry)

# Figure 2: Potential gain in life expectancy from permanently reducing PM2.5 ------ 
# from 2022 concentration to the WHO guideline
# china figure2 dataset
china_fs_fig2_data <- gadm2_aqli_2022 %>%
  mutate(llpp_who_2022 = ifelse(pm2022 <= whostandard, 0, llpp_who_2022)) %>%
  inner_join(china_wv_gadm2, by = c("objectid_gadm2" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2022") %>% 
  select(-geometry, geometry) %>%
  st_as_sf()

# china figure 2
china_fs_fig2 <- china_fs_fig2_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = china_wv_gadm1, color = "azure4", fill = "transparent", lwd = 0.15) +
  geom_sf(data = china_gadm0, color = "cornsilk4", fill = "transparent", lwd = 0.5) +
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
        legend.justification = "center", 
        legend.background = element_rect(color = "#222222"),
        legend.text = element_text(size = 18, color = "#222222"),
        legend.title = element_text(size = 20, color = "#222222"),
        plot.title = element_text(hjust = 0.5, size = 15),
        # legend.key = element_rect(color = "black"),
        legend.box.margin = margin(b = 1, unit = "cm"),
        plot.subtitle = element_text(hjust = 0.5, size = 7),
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
        legend.key = element_rect(color = "#222222"),
        legend.box.spacing = unit(2, "cm"),
        legend.direction = "horizontal",
        plot.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_legend(nrow = 1))


