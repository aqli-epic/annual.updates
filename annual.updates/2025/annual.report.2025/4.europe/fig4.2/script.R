# read in the helper file
source("R/july.2025.helper.script.R")

# exclude the following countries to keep the map less wide and to show a stark 
# difference between eastern and western europe
exclude_countries <-  c("Russia", "Turkey", "Sweden", "Finland", "Norway", 
                        "Kazakhstan", "Iceland", "Georgia", "Azerbaijan", 
                        "Armenia", "Cyprus", "Northern Cyprus", 
                        "Svalbard and Jan Mayen")

countries_except_excluded <- european_countries %>% 
  filter(Country %notin% exclude_countries)

europe_gadm1_shp <- gadm1_aqli_2023_shp %>% 
  filter(name0 %in% unlist(countries_except_excluded)) %>% 
  filter(!(name0 == "Spain" & name1 == "Islas Canarias")) %>%
  filter(!(name0 == "Portugal" & name1 == "Azores")) %>%
  filter(!(name0 == "Portugal" & name1 == "Madeira"))

# very important: this code is run to obtain a country level shapefile of Europe without
# Islas Canarias, Azores and Madeira
europe_gadm0_shp <- europe_gadm1_shp %>% 
  count(name0)

# very important: this code is run to obtain a country level shapefile of 
# Europe without Islas Canarias, Azores and Madeira
europe_gadm0_shp <- europe_gadm1_shp %>% 
  count(name0)

# Europe figure 4.2 ============
# generate large east and west Europe polygons, then find their intersection to 
# mark their border
eastern_europe_large_polygon <- europe_gadm1_shp %>%
  filter(name0 %notin% western_european_countries) %>% 
  st_union()

western_europe_large_polygon <- europe_gadm1_shp %>%
  filter(name0 %in% western_european_countries) %>% 
  st_union()

east_west_border <- st_intersection(eastern_europe_large_polygon, western_europe_large_polygon, model = "closed")

# europe dataset
ar_eur_fig4.2_data <- gadm2_aqli_2023 %>%
  filter(country %in% unlist(countries_except_excluded), !is.na(llpp_who_2023)) %>%
  filter(!(country == "Spain" & name_1 == "Islas Canarias")) %>%
  filter(!(country == "Portugal" & name_1 == "Azores")) %>%
  filter(!(country == "Portugal" & name_1 == "Madeira")) %>%
  left_join(gadm2_aqli_2023_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2023") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# plot
ar_eur_fig4.2 <- ar_eur_fig4.2_data %>%
  filter(country %notin% c("Svalbard and Jan Mayen")) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = europe_gadm1_shp, color = "azure4", fill = "transparent", lwd = 0.1) +
  geom_sf(data = europe_gadm0_shp, color = "cornsilk4", fill = "transparent", lwd = 0.3) +
  geom_sf(data = east_west_border, color = "black", fill = NA, lwd = 1) +
  ggthemes::theme_map()  +
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
  annotate("text", x = 33, y = 61, label = "Eastern Europe: 11.7 µg/m³, \n7.9 months potential gain") +
  annotate("text", x =-11, y = 35, label = "Western Europe: 7.9 µg/m³, \n3.4 months potential gain") +
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