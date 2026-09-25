# read in the helper file
source("~/R/july.2025.helper.script.R")
# Global section figure 1.2 ============
# figure 1.2 trendlines data
trendlines_aqli_data_global <- gadm2_aqli_2023 %>%
  filter(!is.na(population)) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm2016_weighted:pm2023_weighted , names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "Global") %>%
  select(region, years, pop_weighted_avg_pm2.5)

ar_global_fig1.2_data <- gadm2_aqli_2023 %>%
  filter(!is.na(population)) %>%
  mutate(region = case_when(
    country %in% south_asia_def ~ "South Asia",
    country %in% central_and_west_african_countries ~ "Central and west africa",
    country %in% se_asia_vec ~ "South East Asia",
    country %in% latin_america_countries_vec ~ "Latin America",
    country %in% eu_countries ~ "European Union",
    country == "China" ~ "China",
    country %in% mena_countries ~ "Middle East & North Africa",
    TRUE ~ "Rest of the World" # Default case for all others
  ))

#european_countries western_european_countries eu_countries latin_america_countries_vec  
# convert data from wide to long
ar_global_fig1.2_data <- ar_global_fig1.2_data %>%
  group_by(region) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum))%>%
 pivot_longer(cols = pm2016_weighted:pm2023_weighted, names_to = "years",
              values_to = "pop_weighted_avg_pm2.5") %>%
 mutate(years = as.integer(unlist(str_extract(years, "\\d+"))))

# add global trendlines data to the above dataset
ar_global_fig1.2_data <- ar_global_fig1.2_data %>%
  rbind(trendlines_aqli_data_global)
# write.csv(ar_global_fig1.2_data,"C:/Users/HP/Downloads/ar_global_fig1.2_data_final.csv")
ar_global_fig1.2_data$region <- factor(ar_global_fig1.2_data$region, levels = c("South Asia", "Middle East & North Africa", "China","Central and west africa ", "South East Asia", "Latin America", "European Union", "Rest of the World", "Global"))

ar_global_fig1.2_data <- ar_global_fig1.2_data %>% 
  filter(!is.na(region))

ar_global_fig1.2 <- ar_global_fig1.2_data %>%
  ggplot() +  
  geom_line(mapping = aes(x = years, y = pop_weighted_avg_pm2.5, color = region, linetype=region), size = 2) +
  labs(x = "Year", y = expression("Annual Average " ~ PM[2.5] ~ " concentrations (in µg/m³)"), 
       color = "") +
  ggthemes::theme_fivethirtyeight() +
  scale_color_manual(values = c("South Asia" = "#800026", 
                                "Middle East & North Africa" = "#8ea75b", 
                                "China" = "#D63333", 
                                "Global" = "#5e92a9", 
                                "Central and west africa "="#000000", 
                                "South East Asia"= "#f29e37", 
                                "Latin America"="#451F59", 
                                "European Union"= "#fd8d3c",
                                "Rest of the World"="green")) +
  scale_linetype_manual(values = c("South Asia" = "dashed",
                                   "Middle East & North Africa" = "dashed",
                                   "China" = "dashed", 
                                   "Global" = "solid",
                                   "Central and west africa "= "dashed", 
                                   "South East Asia"= "dashed", 
                                   "Latin America"= "dashed", 
                                   "European Union"= "dashed",
                                   "Rest of the World"="dashed")) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 7), 
        axis.title.y = element_text(size = 9), 
        axis.title.x = element_text(size = 9)) +
  scale_y_continuous(breaks = seq(0, 60, 10), limits = c(0, 60)) +
  scale_x_continuous(breaks = c(seq(2016, 2019, 3), 2023)) +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "dashed", "solid", "dashed", "dashed", "dashed", "dashed", "dashed"))),
         linetype = "none") +
  themes_aqli_base +
  theme(plot.background = element_rect(fill = "white", color = "white"), 
        legend.title = element_blank(), 
        legend.key.width = unit(1, "cm"))