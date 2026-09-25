# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


# Filtering color dataset for India
color_2021_india <- gadm2_aqli_2021 %>%
  filter(country == "India") 

# adding a north India/rest of india region column (specifically Indo gangetic plain)
north_india <- c("West Bengal", "Uttar Pradesh", "Punjab", "Haryana", 
                 "Chandigarh", "Bihar", "NCT of Delhi")

# adding North India column
color_2021_india <- color_2021_india %>%
  mutate(region = ifelse(name_1 %in% north_india, "Northern plains of India", "All other regions (excluding Northern plains of India)"))


# plot 1: 2 regions map (North India, All other regions)
plt <- india_state %>%
    mutate(region = ifelse(NAME_1 %in% north_india, "Northern Plains of India", "Other States and UTs")) %>%
    select(-geometry, geometry) %>%
    st_as_sf() %>%
    ggplot() +
    geom_sf(mapping = aes(fill = region), lwd = 0.8) +
    scale_fill_manual(values = c("Northern Plains of India" = "slategray2", "Other States and UTs" = "lightgrey")) +
      labs(fill = "") + 
  ggthemes::theme_tufte() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 13),
        axis.line = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        plot.background = element_rect(fill = "white", color = "white"))
  

