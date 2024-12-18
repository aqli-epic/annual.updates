# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


# central africa definition
central_african_countries <- c("Angola", "Burundi", "Cameroon", 
                                        "Central African Republic", "Chad", 
                                        "Republic of the Congo",
                                        "Democratic Republic of the Congo", 
                                        "Equatorial Guinea", "Gabon",
                                        "São Tomé and Príncipe", 
                                        "Rwanda")
# west africa definition
west_african_countries <- c("Benin", "Burkina Faso", "Cabo Verde", 
                            "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
                            "Côte d'Ivoire", "Liberia", "Mali", "Mauritania", 
                            "Niger", "Nigeria", "Senegal", "Sierra Leone", 
                            "Togo")

# central and west africa countries definition, combine in a single vector
central_and_west_african_countries <- c(central_african_countries, west_african_countries)

# central and west african countries
gadm2_aqli_2021_cw_african <- gadm2_aqli_2021 %>%
  filter(country %in% central_and_west_african_countries) %>%
  mutate(name_1_country = str_c(name_1, " (", country, ")", sep = ""), 
         region = ifelse(country %in% central_african_countries, "Central African", 
                         "West African")) 


#> Figure 4: Average PM2.5 concentration in India from 1998 to 2021------------------------


# creating region wise average PM2.5 data from 1998 to 2021 
cwafrica_fs_fig4_data <- gadm2_aqli_2021_cw_african %>%
  filter(!is.na(population)) %>%
  group_by(region) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2021_weighted, names_to = "years", 
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  select(years, region, pop_weighted_avg_pm2.5)



# adding factor levels
cwafrica_fs_fig4_data$region <- factor(cwafrica_fs_fig4_data$region, levels = c("Central African", "West African"))

# fig 4
cwafrica_fs_fig4 <- cwafrica_fs_fig4_data %>%
  ggplot() +
  geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5, color = region), lwd = 1.3) +
    geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 40)) +
  scale_x_continuous(breaks = c(seq(1998, 2019, 3), 2021)) +
    scale_color_manual(values = c("Central African" = "#7197be", "West African" = "#82b5d5")) +
  ggthemes::theme_tufte() +
  labs(x = "Year", 
       y = expression("Annual Average   " ~ PM[2.5] ~ " Concentration (in µg/m³)"), 
       title = "") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 13, margin = margin(r = 0.6, unit = "cm")), 
        axis.title.x = element_text(size = 13, margin = margin(t = 0.6, b = 0.6, unit = "cm")), 
        axis.line = element_line(), 
        legend.box.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 0.7, unit = "cm")), 
        plot.subtitle = element_text(hjust = 0.5, size = 8, face = "italic"), 
        plot.caption = element_text(size = 7, margin = margin(t = 0.8, unit = "cm"), hjust = 0, face = "italic"),
        axis.text = element_text(size = 12), 
        plot.background = element_rect(color = "white"), 
        axis.ticks = element_blank()) +
     geom_text(x = 2000.5, y = 6, label = expression("WHO" ~ PM[2.5] ~ "Guideline (last updated: 2021): 5 µg/m³"), size = 4.5) 



