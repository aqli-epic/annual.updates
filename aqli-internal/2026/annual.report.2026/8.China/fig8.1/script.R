# ------------------------------------------------------------------------------
# AQLI 2026 – China PM2.5 Life Expectancy Map
# Description : Generates a choropleth map showing potential gain in life 
#               expectancy (LYL) across China at GADM2 level.
#
# Developed By: Purushottam Gupta
# Role        : Data Architect/Data Analyst, AQLI (University of Chicago)
# Contact     : guptap@uchicago.edu
# Dependencies: dplyr, sf, ggplot2, forcats, ggthemes

# ------------------------------------------------------------------------------
# Load Dependencies & Source Files
# ------------------------------------------------------------------------------
source("~/R/helper_function_2026.R")

# ------------------------------------------------------------------------------
# Data Preparation: China (GADM2 with LYL Buckets)
# ------------------------------------------------------------------------------
china_map_data <- gadm2_aqli_2024 %>%
  filter(country == "China") %>%
  left_join(
    gadm2_aqli_2024_shp,
    by = c("country" = "name0", "name_1" = "name1", "name_2" = "name2")
  ) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2024") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# ------------------------------------------------------------------------------
# Administrative Boundaries
# ------------------------------------------------------------------------------
china_lvl1 <- gadm1_aqli_2024_shp %>%
  filter(name0 == "China")

china_lvl0 <- gadm0_aqli_2024_shp %>%
  filter(name0 == "China")

# ------------------------------------------------------------------------------
# Visualization: LYL Choropleth Map
# ------------------------------------------------------------------------------
china_lyl_map <- ggplot(china_map_data) +
  
  # --- GADM2 Fill Layer ---
  geom_sf(
    aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)),
    color = "aliceblue",
    linewidth = 0.05
  ) +
  
  # --- Administrative Boundaries ---
  geom_sf(
    data = china_lvl1,
    color = "azure4",
    fill = NA,
    linewidth = 0.1
  ) +
  
  geom_sf(
    data = china_lvl0,
    color = "cornsilk4",
    fill = NA,
    linewidth = 0.3
  ) +
  
  # --- Color Scale ---
  scale_fill_manual(
    values = c(
      "0 to < 0.1" = "#FFFFFF",
      "0.1 to < 0.5" = "#FFF2E1",
      "0.5 to < 1" = "#FFEDD3",
      "1 to < 2" = "#FFC97A",
      "2 to < 3" = "#FFA521",
      "3 to < 4" = "#EB6C2A",
      "4 to < 5" = "#D63333",
      "5 to < 6" = "#8E2946",
      ">= 6"       = "#451F59"
    )
  ) +
  
  # --- Labels ---
  labs(
    fill = "Potential gain in life expectancy (Years)",
    title = "",
    subtitle = ""
  ) +
  
  # --- Theme ---
  ggthemes::theme_map() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 14),
    
    legend.key = element_rect(color = "black"),
    legend.background = element_rect(color = "black"),
    legend.box.spacing = unit(2, "cm"),
    
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
    
    plot.background = element_rect(fill = "white", color = "white")
  ) +
  
  # --- Legend Layout ---
  guides(fill = guide_legend(nrow = 1))

# ------------------------------------------------------------------------------
# Export Plot
# ------------------------------------------------------------------------------
ggsave(
  filename = "~/Desktop/Final plots aqli/china/ar_china_fig6.1.png",
  plot = china_lyl_map,
  width = 14,
  height = 9,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "~/Desktop/Final plots aqli/china/ar_china_fig6.1.svg",
  plot = china_lyl_map,
  width = 14,
  height = 9,
  units = "in",
  bg = "white"
)