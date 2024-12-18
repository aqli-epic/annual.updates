# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


# central africa definition
central_african_countries <- c("Angola", "Burundi", "Cameroon", 
                                        "Central African Republic", "Chad", 
                                        "Republic of the Congo",
                                        "Democratic Republic of the Congo", 
                                        "Equatorial Guinea", "Gabon",
                                        "São Tomé and Príncipe", 
                                        "Rwanda")
# west africa definition
west_african_countries <- c("Benin", "Burkina Faso", "Cabo Verde", 
                            "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
                            "Côte d'Ivoire", "Liberia", "Mali", "Mauritania", 
                            "Niger", "Nigeria", "Senegal", "Sierra Leone", 
                            "Togo")

# central and west africa countries definition, combine in a single vector
central_and_west_african_countries <- c(central_african_countries, west_african_countries)

# central and west african countries
gadm2_aqli_2021_cw_african <- gadm2_aqli_2021 %>%
  filter(country %in% central_and_west_african_countries) %>%
  mutate(name_1_country = str_c(name_1, " (", country, ")", sep = ""), 
         region = ifelse(country %in% central_african_countries, "Central African", 
                         "West African")) 


#> plot 1: map screenshot--------------------------------


# cw africa figure1 dataset
cw_africa_fs_fig1_data <- gadm2_aqli_2021_cw_african %>%
  left_join(gadm2_aqli_2021_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2021") %>%
  select(-geometry, geometry) %>%
  st_as_sf()


# cw africa fig1 (burlywood4)
cw_africa_fig1 <- cw_africa_fs_fig1_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
   geom_sf(data = gadm1_aqli_2021_shp %>% filter(name0 %in% central_and_west_african_countries), color = "azure4", fill = "transparent", lwd = 0.1) +
 geom_sf(data = gadm0_aqli_2021_shp %>% filter(name0 %in% central_and_west_african_countries), color = "cornsilk4", fill = "transparent", lwd = 0.3) +
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

