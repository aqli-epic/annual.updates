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

# Figure 1: Improvements in life expectancy due to reduced pollution between 2014 and 2022 ------
# china figure1 dataset
china_fs_fig1_data <- gadm2_aqli_2022 %>%
  inner_join(china_wv_gadm2, by = c("objectid_gadm2" = "obidgadm2")) %>%
  mutate(lyl2014minus2022 = round((pm2014 - pm2022)*0.098, 1)) %>% # to reflect gain in life expectancy
  add_aqli_color_scale_buckets(scale_type = "lyldiff", col_name = "lyl2014minus2022") %>% 
  select(-geometry, geometry) %>%
  st_as_sf()

# china figure 1
china_fs_fig1 <- china_fs_fig1_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyldiff_bucket, order_lyldiff_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = china_wv_gadm1, color = "azure4", fill = "transparent", lwd = 0.15) +
  geom_sf(data = china_wv_gadm0, color = "cornsilk4", fill = "transparent", lwd = 0.5) +
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
  labs(fill = "Change in life expectancy between 2014 and 2022 (Years; blue values indicate improvement)", title = "") +
  theme(legend.position = "bottom", 
        legend.justification = "center", 
        legend.background = element_rect(color = "#222222"),
        legend.text = element_text(size = 15, color="#222222"),
        legend.title = element_text(size = 16, color="#222222"),
        plot.title = element_text(hjust = 0.5, size = 15),
        # legend.key = element_rect(color = "black"),
        legend.box.margin = margin(b = 1, unit = "cm"),
        plot.subtitle = element_text(hjust = 0.5, size = 7),
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
        legend.key = element_rect(color = "black"),
        legend.box.spacing = unit(2, "cm"),
        legend.direction = "horizontal",
        plot.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_legend(nrow = 1))


