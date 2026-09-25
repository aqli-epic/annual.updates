# ------------------------------------------------------------------------------
# AQLI 2026 – Europe Change in Life Expectancy Map
# Description : Visualizes change in life expectancy (1998–2024) across Europe, 
#               highlighting east-west disparities.
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
# Data Preparation: Europe Shapefiles
# ------------------------------------------------------------------------------

# Exclude the following countries to keep the map less wide and to show a stark 
# difference between eastern and western Europe
exclude_countries <- c(
  "Russia", "Turkey", "Sweden", "Finland", "Norway",
  "Kazakhstan", "Iceland", "Georgia", "Azerbaijan",
  "Armenia", "Cyprus", "Northern Cyprus",
  "Svalbard and Jan Mayen"
)

countries_filtered <- setdiff(europe_countries, exclude_countries)

europe_gadm1_shp <- gadm1_aqli_2024_shp %>%
  filter(name0 %in% countries_filtered) %>%
  filter(!(name0 == "Spain" & name1 == "Islas Canarias")) %>%
  filter(!(name0 == "Portugal" & name1 == "Azores")) %>%
  filter(!(name0 == "Portugal" & name1 == "Madeira"))

# Very important: this code is run to obtain a country level shapefile of Europe without
# Islas Canarias, Azores and Madeira
europe_gadm0_shp <- europe_gadm1_shp %>%
  count(name0)

# ------------------------------------------------------------------------------
# Data Preparation: LYL Buckets
# ------------------------------------------------------------------------------
europe_lyl_data <- ar_eur_fig9.1_data %>%
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
    ),
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
europe_lyl_map <- ggplot(europe_lyl_data) +
  geom_sf(
    aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)),
    color = "aliceblue",
    linewidth = 0.05
  ) +
  geom_sf(
    data = europe_gadm1_shp,
    color = "azure4",
    fill = NA,
    linewidth = 0.1
  ) +
  geom_sf(
    data = europe_gadm0_shp,
    color = "cornsilk4",
    fill = NA,
    linewidth = 0.3
  ) +
  ggthemes::theme_map() +
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
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 14),
    legend.key = element_rect(color = "black"),
    legend.background = element_rect(color = "black"),
    legend.box.margin = margin(b = 1, unit = "cm"),
    legend.box.spacing = unit(0, "cm"),
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
    plot.background = element_rect(fill = "white", color = "white")
  ) +
  guides(fill = guide_legend(nrow = 1))

# ------------------------------------------------------------------------------
# End Here
# ------------------------------------------------------------------------------
