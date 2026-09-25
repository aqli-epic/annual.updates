# read in the helper file
source("~/Downloads/annual.updates/R/july.2025.helper.script.R") 
library(patchwork)

# Define vectors
oceania_vec_east <- c("Australia","Christmas Island","Cocos Islands","Guam","Heard Island and McDonald Island","Marshall Islands",
                      "Micronesia","Nauru","Northern Mariana Islands","Norfolk Island","New Caledonia","Palau",
                      "Papua New Guinea","Solomon Islands","Tuvalu","Vanuatu","New Zealand","Fiji","Kiribati")
oceania_vec_west <- c("American Samoa","Cook Islands","French Polynesia","Samoa","Tonga","Tokelau",
                      "Wallis and Futuna")



plot_oceania_map <- function(region_vec, title_text, show_legend = TRUE, exclude_subregions = NULL) {
  # Step 1: Filter and rename CSV fields
  df_filtered <- gadm2_aqli_2023 %>%
    filter(country %in% region_vec,!is.na(llpp_who_2023)) %>%
    rename(name1 = name_1, name2 = name_2)
  
  # Step 2: Join shapefile with suffixes
  plot_data <- df_filtered %>%
    left_join(gadm2_aqli_2023_shp, by = c("objectid_gadm2" = "obidgadm2"), suffix = c(".csv", ".shp")) %>%
    add_aqli_color_scale_buckets("lyl", "llpp_who_2023") %>%
    st_as_sf()
  
  # Step 3: Clean and filter excluded subregions
  if (!is.null(exclude_subregions)) {
    exclude_clean <- tolower(trimws(exclude_subregions))
    
    plot_data <- plot_data %>%
      mutate(
        name1_clean = tolower(trimws(name1.csv)),
        name2_clean = tolower(trimws(name2.csv))
      ) %>%
      filter(!(name1_clean %in% exclude_clean | name2_clean %in% exclude_clean))
  }
  
  # Step 4: Filter GADM1 for borders (remove excluded islands)
  gadm1_filtered <- gadm1_aqli_2023_shp %>%
    filter(name0 %in% region_vec)
  
  if (!is.null(exclude_subregions)) {
    gadm1_filtered <- gadm1_filtered %>%
      mutate(
        name1_clean = tolower(trimws(name1))
      ) %>%
      filter(!(name1_clean %in% exclude_clean))
  }
  
  # Step 5: Plot
  plot <- ggplot(plot_data) +
    geom_sf(aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)),
            color = "aliceblue", lwd = 0.05) +
    geom_sf(data = gadm1_filtered,
            color = "azure4", fill = NA, lwd = 0.1) +
    geom_sf(data = gadm0_aqli_2023_shp %>%
              filter(name0 %in% region_vec),
            color = "cornsilk4", fill = NA, lwd = 0.3) +
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
    labs(title = title_text, fill = "Potential gain in life expectancy (Years)") +
    theme(
      legend.position = ifelse(show_legend, "bottom", "none"),
      legend.justification = c(0.5, 3),
      legend.background = element_rect(color = "black"),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 15),
      plot.title = element_text(hjust = 0.5, size = 15),
      legend.key = element_rect(color = "black"),
      legend.box.margin = margin(b = 1, unit = "cm"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
      legend.box.spacing = unit(2, "cm"),
      legend.direction = "horizontal"
    )
  
  if (title_text == "East Oceania") {
    plot <- plot + coord_sf(xlim = c(110, 180), ylim = c(-50, 0), expand = FALSE)
  }
  return(plot)
}




# Create individual plots: East plot with legend, West plot without legend
east_plot <- plot_oceania_map(oceania_vec_east, "East Oceania", show_legend = TRUE, exclude_subregions=c("Chatham Islands","Kermadec Islands","Auckland islands","Campbell Islands"))
west_plot <- plot_oceania_map(oceania_vec_west, "West Oceania")+
  guides(fill = "none")

# Use patchwork to combine the plots (this is where the issue lies)
ar_oce_fig9.2 <- east_plot + west_plot + 
  plot_layout(guides = "collect")&
  theme(legend.position = "bottom", legend.justification = "center")