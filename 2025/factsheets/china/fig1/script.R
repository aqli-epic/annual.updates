# read in the helper file
source("~/R/july.2025.helper.script.R")

# China figure 1 ============
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

china_gadm0 <- st_read("~/shapefile/AQLI Shapefiles/China_Worldview/gadm0_China_ChnWV/china_gadm0.shp") %>%
  mutate(name0 = "China",
         obidgadm0 = 46,
         isoalp3 = "CHN") %>%
  select(obidgadm0, isoalp3, name0, geometry)
# china figure 1 ===========
# data
fs_china_fig1_data <- gadm2_aqli_2023 %>%
  #inner_join(china_wv_gadm2, by = c("objectid_gadm2" = "obidgadm2")) %>%
  filter(country == c("China")) %>%
  left_join(gadm2_aqli_2023_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2023") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# plot
fs_china_fig1 <- fs_china_fig1_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = gadm1_aqli_2023_shp %>% filter(name0 == c("China")), color = "azure4", fill = "transparent", lwd = 0.1) +
  geom_sf(data = gadm0_aqli_2023_shp %>% filter(name0 == c("China")), color = "cornsilk4", fill = "transparent", lwd = 0.3) +
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
  labs(fill = "Potential gain in life expectancy (Years)  ", title = "",
       subtitle = "") +
  theme(legend.position = "bottom",
        legend.justification = c(0.5, 3),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 15),
        # legend.key = element_rect(color = "black"),
        legend.box.margin = margin(b = 1, unit = "cm"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
        legend.key = element_rect(color = "black"),
        legend.box.spacing = unit(2, "cm"),
        legend.direction = "horizontal",
        plot.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_legend(nrow = 1))