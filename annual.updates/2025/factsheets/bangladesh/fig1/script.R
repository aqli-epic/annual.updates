# read in the helper file
source("~/R/july.2025.helper.script.R")

#' Fig 1: Potential gain in Life Expectancy from permanently reducing PM2.5 from --------
#' 2023 levels to the WHO PM2.5 guideline.

# bangladesh figure1 dataset
bangladesh_fs_fig1_data <- gadm2_aqli_2023 %>%
  filter(country == "Bangladesh") %>%
  left_join(gadm2_aqli_2023_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2023") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# bangladesh figure 1
bangladesh_fs_fig1 <- bangladesh_fs_fig1_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "lightgrey", lwd = 0.05) +
  geom_sf(data = gadm1_aqli_2023_shp %>% filter(name0 == "Bangladesh"), color = "black", fill = "transparent", lwd = 0.15) +
  ggthemes::theme_map() +
  scale_fill_manual(values = c(
    "0 to < 0.1" = "#FFFFFF", 
    "0.1 to < 0.5" = "#FFF2E1", 
    "0.5 to < 1" = "#FFEDD3", 
    "1 to < 2" = "#FFC97A", 
    "2 to < 3" = "#FFA521", 
    "3 to < 4" = "#EB6C2A", 
    "4 to < 5" = "#D63333", 
    "5 to < 6" = "#8E2946", 
    ">= 6" = "#451F59")) +
  ggthemes::theme_map() +
  labs(fill = "Potential gain in life expectancy (Years)  ", title = "") +
  theme(legend.position = "bottom",
        legend.justification = c(0.5, 0),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 24, color="#222222"),
        legend.title = element_text(size = 24, color="#222222"),
        plot.title = element_text(hjust = 0.5, size = 16),
        # legend.key = element_rect(color = "black"),
        legend.box.margin = margin(b = 1, unit = "cm"),
        plot.subtitle = element_text(hjust = 0.5, size = 7),
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
        legend.key = element_rect(color = "black"),
        legend.box.spacing = unit(0, "cm"),
        legend.direction = "horizontal",
       plot.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_legend(nrow = 1))