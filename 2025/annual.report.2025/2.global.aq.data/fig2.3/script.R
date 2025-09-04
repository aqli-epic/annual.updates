# read in the helper file
source("~/R/july.2025.helper.script.R")

# global aq data figure 2.3 ======
# data
public_data <- read_csv("~/Desktop/AQLI/2025 AQLI Update/data/open_data.csv")
awardees <- read_excel("~/Desktop/AQLI/2025 AQLI Update/data/Awardee Info.xlsx", skip = 2)

# figure 2.3 data
ar_global_fig2.3_data <- awardees %>%
  rename("country" = "Country in which project is being executed") %>%
  mutate(country = ifelse(country == "Cote d'Ivoire", "Côte d'Ivoire", country),
         country = ifelse(country == "The Gambia", "Gambia", country)) %>%
  group_by(country) %>%
  summarise(num_awards = n()) %>%
  right_join(public_data, by = c("country" = "Country or Dependency")) %>%
  inner_join(gadm0_aqli_shp, by = c("country" = "name0")) %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# plot
ar_global_fig2.3 <- ggplot() +
  geom_sf(data = ar_global_fig2.3_data, 
          mapping = aes(fill = `Is there any evidence of current government operated AQ monitoring system in 2024?`), 
          color = "white", 
          lwd = 0.3) + 
  geom_sf(data = gadm0_aqli_shp, 
          color = "cornsilk4", 
          fill = "transparent", 
          lwd = 0.3) +
  geom_sf(data = filter(ar_global_fig2.3_data, !is.na(num_awards)),
          fill = NA,            
          color = "#fed976",     
          lwd = 0.3,           
          show.legend = FALSE) +
  ggthemes::theme_map() +
  scale_fill_manual(name = "Is there any evidence of current government operated AQ monitoring system in 2024?",
                    values = c("No" = "#800026",
                               "Yes" = "#5e92a9")) +
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
