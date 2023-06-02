
# read in the helper file (look for appPublic folder in the root of the repository)
source("./appPublic/aqli.data.explorer.helper.script.R")


# south korea figure1 dataset
south_korea_fs_fig1_data <- gadm2_aqli_2021 %>%
  filter(country == "South Korea") %>%
  left_join(gadm2_aqli_2021_shp, by = c("objectid_gadm2" = "objidgadm2")) %>%
  mutate(lyl_bucket = ifelse((llpp_who_2021 >= 0) & (llpp_who_2021 < 0.1), "0 - < 0.1", NA), 
         lyl_bucket = ifelse((llpp_who_2021 >= 0.1) & (llpp_who_2021 <= 0.5), "0.1 - 0.5", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 0.5) & (llpp_who_2021 <= 1), "> 0.5 - 1", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 1) & (llpp_who_2021 <= 2), "> 1 - 2", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 2) & (llpp_who_2021 <= 3), "> 2 - 3", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 3) & (llpp_who_2021 <= 4), "> 3 - 4", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 4) & (llpp_who_2021 <= 5), "> 4 - 5", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 5) & (llpp_who_2021 < 6), "> 5 - < 6", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 >= 6), ">= 6", lyl_bucket)) %>%
  mutate(order_lyl_bucket = ifelse(lyl_bucket == "0 - < 0.1", 1, NA), 
         order_lyl_bucket = ifelse(lyl_bucket == "0.1 - 0.5", 2, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 0.5 - 1", 3, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 1 - 2", 4, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 2 - 3", 5, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 3 - 4", 6, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 4 - 5", 7, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 5 - < 6", 8, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == ">= 6", 9, order_lyl_bucket)) %>%
  select(-geometry, geometry) %>%
  st_as_sf()

south_korea_fs_fig1_data %>%
  sf::write_sf("./september.2023/factsheets/southkorea/fig1/sk_fs_fig1.shp")


# south korea figure 1
south_korea_fs_fig1 <- south_korea_fs_fig1_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "transparent") +
  geom_sf(data = gadm1_aqli_2021_shp %>% filter(name0 == "South Korea"), color = "black", fill = "transparent", lwd = 0.5) +
  ggthemes::theme_map() + 
 scale_fill_manual(values = c("0 - < 0.1 years" = "#FFFFFF", 
                               "0.1 - 0.5" = "#FFE6B3", 
                               "> 0.5 - 1" = "#FFD25D", 
                               "> 1 - 2" = "#FFBA00", 
                               "> 2 - 3" = "#FF9600", 
                               "> 3 - 4" = "#FF6908", 
                               "> 4 - 5" = "#E63D23", 
                               "> 5 - < 6" = "#BD251C", 
                               ">= 6" = "#8C130E")) +
  ggthemes::theme_map() +
  labs(fill = "Life years lost  ", title = expression("Potential Gains in Life Expectancy through Permanently Reducing" ~ PM[2.5] ~ "from 2021 concentrations to the WHO guideline")) + 
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
        legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1))

