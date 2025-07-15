# read in the helper file
source("~/R/july.2025.helper.script.R")
public_data <- read_csv("~/Desktop/AQLI/2025 AQLI Update/data/open_data.csv")

# global aq data figure 2.2 ======
# data
ar_global_fig2.2_data <- public_data %>%
  inner_join(gadm0_aqli_shp, by = c("Country or Dependency" = "name0")) %>%
  select(-geometry, geometry, ) %>%
  st_as_sf()

# plot
ar_global_fig2.2 <- ggplot() +
  geom_sf(data = gadm0_aqli_shp, 
          color = "cornsilk4", 
          fill = "white", 
          lwd = 0.05) +
  geom_sf(data = ar_global_fig2.2_data, 
          mapping = aes(fill = `Is there any evidence of current government operated AQ monitoring system in 2024?`), 
          color = "white", 
          lwd = 0.05) + 
  ggthemes::theme_map() +
  scale_fill_manual(name = "Is there any evidence of current government operated AQ monitoring system in 2024?",
                    values = c("No" = "#800026",
                               "Yes" = "#5e92a9")) +
  # scale_shape_manual(name = "",
  #                    values = c("Awardee" = 21)) +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.key = element_rect(color = "black"),
        legend.box.margin = margin(b = 1, unit = "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.direction = "horizontal") 

