# ------------------------------------------------------------------------------
# AQLI 2026 – US & Canada Life Expectancy Map
# Description : Visualizes potential gain in life expectancy from meeting WHO 
#               PM2.5 guideline (2024) across subnational regions in the 
#               United States and Canada.
#
# Developed By: Purushottam Gupta
# Role        : Data Architect & Data Analyst, AQLI (University of Chicago)
# Contact     : guptap@uchicago.edu
# Dependencies: dplyr, sf, ggplot2, forcats, ggthemes
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Load Dependencies & Source Files
# ------------------------------------------------------------------------------
source("~/R/helper_function_2026.R")

# ------------------------------------------------------------------------------
# Data Preparation: US & Canada Subnational Data
# ------------------------------------------------------------------------------

# exclude Alaska and Hawaii for consistent mainland visualization
us_canada_data_fig <- gadm2_aqli_2024 %>%
  filter(
    country %in% c("United States", "Canada"),
    name_1 != "Alaska",
    name_1 != "Hawaii"
  )

# merge with shapefile and compute AQLI buckets
ar_us_canada_fig10.1_data <- us_canada_data_fig %>%
  select(country, name_1, name_2, pm2024, llpp_who_2024) %>%
  filter(!is.na(llpp_who_2024)) %>%
  left_join(
    gadm2_aqli_2024_shp,
    by = c("country" = "name0", "name_1" = "name1", "name_2" = "name2")
  ) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2024") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# ------------------------------------------------------------------------------
# Visualization
# ------------------------------------------------------------------------------
ar_us_canada_fig10.1 <- ar_us_canada_fig10.1_data %>%
  ggplot() +
  geom_sf(
    mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)),
    color = "aliceblue",
    lwd = 0.05
  ) +
  geom_sf(
    data = gadm1_aqli_2024_shp %>%
      filter(
        name0 %in% c("United States", "Canada"),
        name1 != "Alaska",
        name1 != "Hawaii"
      ),
    color = "azure4",
    fill = "transparent",
    lwd = 0.1
  ) +
  geom_sf(
    data = gadm1_aqli_2024_shp %>%
      filter(
        name0 %in% c("United States", "Canada"),
        name1 == "California",
        name1 != "Alaska",
        name1 != "Hawaii"
      ),
    color = "cornsilk4",
    fill = "transparent",
    lwd = 0.3
  ) +
  ggthemes::theme_map() +
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
      ">= 6" = "#451F59"
    )
  ) +
  ggthemes::theme_map() +
  labs(
    fill = "Potential gain in life expectancy (Years)",
    title = "",
    subtitle = ""
  ) +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 3),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, size = 15),
    legend.box.margin = ggplot2::margin(b = 1, unit = "cm"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
    legend.key = element_rect(color = "black"),
    legend.box.spacing = unit(2, "cm"),
    legend.direction = "horizontal",
    plot.background = element_rect(fill = "white", color = "white")
  ) +
  guides(fill = guide_legend(nrow = 1))

# ------------------------------------------------------------------------------
# End Here
# ------------------------------------------------------------------------------
