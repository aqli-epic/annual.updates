# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


# Filtering color dataset for India
color_2021_india <- gadm2_aqli_2021 %>%
  filter(country == "India") 

# adding a north India/rest of india region column (specifically Indo gangetic plain)
north_india <- c("West Bengal", "Uttar Pradesh", "Punjab", "Haryana", 
                 "Chandigarh", "Bihar", "NCT of Delhi")

# adding North India column
color_2021_india <- color_2021_india %>%
  mutate(region = ifelse(name_1 %in% north_india, "Northern plains of India", "All other regions (excluding Northern plains of India)"))


   
#> plot 2: AQLI map screenshot (I have limited this to the Indo-Gangetic plain as opposed to the current screenshot that shows it for all of India)------------------


#> plot 2: map screenshot--------------------

# north india figure2 dataset
north_india_fs_fig2_data <- gadm2_aqli_2021 %>%
  filter(country == "India", name_1 %in% north_india) %>%
  left_join(gadm2_aqli_2021_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2021") %>%
  select(-geometry, geometry) %>%
  st_as_sf()



# north india figure 2
north_india_fs_fig2 <- north_india_fs_fig2_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "lightgrey", lwd = 0.05) +
 geom_sf(data = gadm1_aqli_2021_shp %>% filter(name0 == "India", name1 %in% north_india), color = "black", fill = "transparent", lwd = 0.5) +
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
  guides(fill = guide_legend(nrow = 1)) +
   ggsflabel::geom_sf_label_repel(data = gadm1_aqli_2021_shp %>% filter(name0 == "India", name1 %in% north_india), mapping = aes(label = name1), force = 50, nudge_x = 3, seed = 10, size = 4)

  



