# ------------------------------------------------------------------------------
# AQLI 2026 – US & Canada Change in Life Expectancy (1998–2024)
# Description : Visualizes change in life expectancy from PM2.5 exposure 
#               between 1998 and 2024 across subnational regions in the 
#               United States and Canada.
#
# Developed By: Purushottam Gupta
# Role        : Data Architect/Data Analyst, AQLI (University of Chicago)
# Contact     : guptap@uchicago.edu
# Dependencies: dplyr, sf, ggplot2, forcats, ggthemes
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Load Dependencies & Source Files
# ------------------------------------------------------------------------------
source("~/R/helper_function_2026.R")

# ------------------------------------------------------------------------------
# Data Preparation: US & Canada Dataset
# ------------------------------------------------------------------------------

# exclude Alaska and Hawaii for consistent mainland visualization
us_canada_data_fig <- gadm2_aqli_2024 %>%
  filter(
    country %in% c("United States", "Canada"),
    name_1 != "Alaska",
    name_1 != "Hawaii"
  )

# compute change in life expectancy between 1998 and 2024
us_canada_data_fig <- us_canada_data_fig %>%
  mutate(lyl1998minus2024 = llpp_who_1998 - llpp_who_2024)

# shapefile filtered for US & Canada mainland regions
colormap_shp_us_can <- gadm2_aqli_2024_shp %>%
  filter(
    name0 %in% c("United States", "Canada"),
    name1 != "Alaska",
    name1 != "Hawaii"
  )

# merge with shapefile and retain required variables
ar_us_canada_fig10.2_data <- us_canada_data_fig %>%
  select(country, name_1, name_2, pm2024, llpp_who_2024, lyl1998minus2024) %>%
  filter(!is.na(llpp_who_2024)) %>%
  left_join(
    gadm2_aqli_2024_shp,
    by = c("country" = "name0", "name_1" = "name1", "name_2" = "name2")
  ) %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# ------------------------------------------------------------------------------
# Data Preparation: LYL Buckets
# ------------------------------------------------------------------------------
ar_us_canada_fig10.2_data <- ar_us_canada_fig10.2_data %>%
  mutate(
    lyl_bucket = case_when(
      lyl1998minus2024 >= 2 ~ ">= 2",
      lyl1998minus2024 >= 0.5 ~ "0.5 to (< 2)",
      lyl1998minus2024 >= 0.1 ~ "0.1 to (< 0.5)",
      lyl1998minus2024 >= 0 ~ "0 to (< 0.1)",
      lyl1998minus2024 >= -0.1 ~ "-0.1 to (< 0)",
      lyl1998minus2024 >= -0.5 ~ "-0.5 to (< -0.1)",
      lyl1998minus2024 >= -2 ~ "-2 to (< -0.5)",
      TRUE ~ "< -2"
    )
  )

ar_us_canada_fig10.2_data <- ar_us_canada_fig10.2_data %>%
  mutate(
    order_lyl_bucket = case_when(
      lyl_bucket == "< -2" ~ 1,
      lyl_bucket == "-2 to (< -0.5)" ~ 2,
      lyl_bucket == "-0.5 to (< -0.1)" ~ 3,
      lyl_bucket == "-0.1 to (< 0)" ~ 4,
      lyl_bucket == "0 to (< 0.1)" ~ 5,
      lyl_bucket == "0.1 to (< 0.5)" ~ 6,
      lyl_bucket == "0.5 to (< 2)" ~ 7,
      lyl_bucket == ">= 2" ~ 8
    )
  )

# ------------------------------------------------------------------------------
# Visualization
# ------------------------------------------------------------------------------
ar_us_canada_fig10.2 <- ar_us_canada_fig10.2_data %>%
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
  scale_fill_manual(
    values = c(
      ">= 2" = "#008fbb",
      "0.5 to (< 2)" = "#4fb6d3",
      "0.1 to (< 0.5)" = "#99dbe9",
      "0 to (< 0.1)" = "#d2eef4",
      "-0.1 to (< 0)" = "#ffd393",
      "-0.5 to (< -0.1)" = "#fea222",
      "-2 to (< -0.5)" = "#ec6f29",
      "< -2" = "#d63333"
    )
  ) +
  labs(
    fill = "Change in life expectancy between 1998 and 2024 \n(Years; blue values indicate improvement)",
    title = "",
    subtitle = ""
  ) +
  ggthemes::theme_map() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, size = 15),
    legend.box.margin = ggplot2::margin(b = 1, unit = "cm"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
    legend.key = element_rect(color = "black"),
    legend.box.spacing = unit(0, "cm"),
    legend.direction = "horizontal",
    plot.background = element_rect(fill = "white", color = "white")
  ) +
  guides(fill = guide_legend(nrow = 1))