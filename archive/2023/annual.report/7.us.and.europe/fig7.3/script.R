# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")



# figure 7.3 (Europe)===========================================================

# create a europe AQLI dataset
gadm2_aqli_2021_europe <- gadm2_aqli_2021 %>%
  filter(country %in% european_countries$Country)

# exclude the following countries to keep the map less wide and to show a stark difference between eastern and western europe
exclude_countries <-  c("Russia", "Turkey", "Sweden", "Finland", "Norway", "Kazakhstan", "Iceland", "Georgia", "Azerbaijan", "Armenia", "Cyprus", "Northern Cyprus", "Svalbard and Jan Mayen")


countries_except_excluded <- european_countries %>% 
  filter(Country %notin% exclude_countries)

# europe figure1 dataset
europe_1998_2021_diff_data <- gadm2_aqli_2021_europe %>%
  filter(country %notin% exclude_countries) %>%
   mutate(lyl2021minus1998 = round((pm2021 - pm1998)*0.098, 2)) %>%
  left_join(gadm2_aqli_2021_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyldiff", "lyl2021minus1998") %>%
  select(-geometry, geometry) %>%
  st_as_sf()


europe_1998_2021_diff_data <- europe_1998_2021_diff_data %>%
  filter(!(country == "Spain" & name_1 == "Islas Canarias")) %>%
  filter(!(country == "Portugal" & name_1 == "Azores"))


# europe fs fig1
ar_fig7.3 <- europe_1998_2021_diff_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyldiff_bucket, order_lyldiff_bucket)), color = "aliceblue", lwd = 0.05) +
   geom_sf(data = gadm1_aqli_2021_shp %>% filter(name0 %in% unlist(countries_except_excluded)) %>% 
                                                     filter(!(name0 == "Spain" & name1 == "Islas Canarias")) %>%
  filter(!(name0 == "Portugal" & name1 == "Azores")), color = "azure4", fill = "transparent", lwd = 0.1) +
 geom_sf(data = gadm0_aqli_2021_shp %>% filter(name0 %in% unlist(countries_except_excluded)), color = "cornsilk4", fill = "transparent", lwd = 0.3) +
  ggthemes::theme_map()  +
 scale_fill_manual(values = c("< -2" = "#4575b4", 
                               "-2 to (< -0.5)" = "#74add1", 
                               "-0.5 to (< -0.1)" = "#abd9e9", 
                               "-0.1 to (< 0)" = "#e0f3f8", 
                               "0 to (< 0.1)" = "#fee090", 
                               "0.1 to (< 0.5)" = "#fdae61", 
                               "0.5 to (< 2)" = "#f46d43", 
                               ">= 2" = "#d73027")) +
  ggthemes::theme_map() +
  labs(fill = "Change in potential gains in life expectancy between 1998 and 2021 (Years)", title = "", 
       subtitle = "") + 
 theme(legend.position = "bottom", 
        legend.justification = c(0.5, 3), 
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

