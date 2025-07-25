################################################################################
# Ecuador Factsheet – Figure 1: Map Plot
################################################################################

# --- 1. Load helper functions -------------------------------------------------
source("~/R/july.2025.helper.script.R")


# --- 2. Define Inputs ---------------------------------------------------------

# llpp_who_2023: Life expectancy lost (in years) due to PM2.5 exposure above WHO guideline (5 µg/m³), in 2023

# --- 3. Prepare Spatial Dataset for Mapping -----------------------------------

# Join tabular and spatial data, classify by color buckets
ecuador_map_data <- gadm2_aqli_2023 %>%
  filter(country %in% c("Ecuador")) %>%
  left_join(gadm2_aqli_2023_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2023") %>%
  select(-geometry, geometry) %>%
  st_as_sf()


# --- 4. Create Map Plot -------------------------------------------------------

fs_ecuador_fig1 <- ecuador_map_data %>%
  ggplot() +
  
  # Fill districts by life expectancy gain bucket
  geom_sf(
    aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)),
    color = "aliceblue", lwd = 0.05
  ) +
  
  # Overlay provincial boundaries
  geom_sf(
    data = gadm1_aqli_2023_shp %>% filter(name0 %in% c("Ecuador")),
    color = "azure4", fill = "transparent", lwd = 0.1
  ) +
  
  # Overlay national boundaries
  geom_sf(
    data = gadm0_aqli_2023_shp %>% filter(name0 %in% c("Ecuador")),
    color = "cornsilk4", fill = "transparent", lwd = 0.3
  ) +
  ggthemes::theme_map() +
  
  # Manual fill scale based on AQLI bucket color codes
  scale_fill_manual(values = c(
    "0 to < 0.1" = "#FFFFFF", 
    "0.1 to < 0.5" = "#FFF2E1", 
    "0.5 to < 1" = "#FFEDD3", 
    "1 to < 2"   = "#FFC97A", 
    "2 to < 3"   = "#FFA521", 
    "3 to < 4"   = "#EB6C2A", 
    "4 to < 5"   = "#D63333", 
    "5 to < 6"   = "#8E2946", 
    ">= 6"       = "#451F59" )) +
  
  # Titles and subtitles (currently empty)
  labs(
    fill = "Potential gain in life expectancy (Years)",
    title = "",
    subtitle = "" ) +
  
  # Custom theming for legend and layout
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 3),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15),
    legend.key = element_rect(color = "black"),
    legend.box.margin = margin(b = 1, unit = "cm"),
    legend.box.spacing = unit(2, "cm"),
    legend.direction = "horizontal",
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.7, size = 9, face = "italic")) +
  
  # Arrange legend items horizontally
  guides(fill = guide_legend(nrow = 1))





