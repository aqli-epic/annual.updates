# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


# create a europe AQLI dataset
gadm2_aqli_2021_europe <- gadm2_aqli_2021 %>%
  filter(country %in% european_countries$Country)


# europe pop
europe_pop <- sum(gadm2_aqli_2021_europe$population, na.rm =  TRUE)

# pop living above who guideline
europe_above_who <- gadm2_aqli_2021_europe %>%
  filter(pm2021 > 10) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE))


# country wise average and how many are above WHO
europe_country_wise <- gadm2_aqli_2021_europe %>%
  gadm_level_summary(c("country"), c(2021), 10)


# exclude the following countries to keep the map less wide and to show a stark difference between eastern and western europe
exclude_countries <-  c("Russia", "Turkey", "Sweden", "Finland", "Norway", "Kazakhstan", "Iceland", "Georgia", "Azerbaijan", "Armenia", "Cyprus", "Northern Cyprus", "Svalbard and Jan Mayen")


countries_except_excluded <- european_countries %>% 
  filter(Country %notin% exclude_countries)




#> plot 1 (europe screenshot map) -----------------------------------------------------

# exclude the following countries to keep the map less wide and to show a stark difference between eastern and western europe
exclude_countries <-  c("Russia", "Turkey", "Sweden", "Finland", "Norway", "Kazakhstan", "Iceland", "Georgia", "Azerbaijan", "Armenia", "Cyprus", "Northern Cyprus", "Svalbard and Jan Mayen")


countries_except_excluded <- european_countries %>% 
  filter(Country %notin% exclude_countries)

# europe figure1 dataset
europe_fs_fig1_data <- gadm2_aqli_2021_europe %>%
  filter(country %notin% exclude_countries) %>%
  left_join(gadm2_aqli_2021_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2021") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# china_fs_fig1_data %>%
#   sf::write_sf("./september.2023/factsheets/southkorea/fig1/sk_fs_fig1.shp")


europe_fs_fig1_data <- europe_fs_fig1_data %>%
  filter(!(country == "Spain" & name_1 == "Islas Canarias")) %>%
  filter(!(country == "Portugal" & name_1 == "Azores"))


# europe fs fig1
europe_fs_fig1 <- europe_fs_fig1_data %>%
  filter(country %notin% c("Svalbard and Jan Mayen")) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
   geom_sf(data = gadm1_aqli_2021_shp %>% filter(name0 %in% unlist(countries_except_excluded)) %>% 
                                                     filter(!(name0 == "Spain" & name1 == "Islas Canarias")) %>%
  filter(!(name0 == "Portugal" & name1 == "Azores")), color = "azure4", fill = "transparent", lwd = 0.1) +
 geom_sf(data = gadm0_aqli_2021_shp %>% filter(name0 %in% unlist(countries_except_excluded)), color = "cornsilk4", fill = "transparent", lwd = 0.3) +
  ggthemes::theme_map()  +
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
  labs(fill = "Potential gain in life expectancy (Years)  ", title = "", 
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


