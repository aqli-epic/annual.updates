# read in the helper file
source("R/july.2024.helper.script.R")

# Global section figure 1.2 ============
# figure 1.2 trendlines data
trendlines_aqli_data_global <- gadm2_aqli_2022 %>%
  filter(!is.na(population)) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2022_weighted , names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "Global") %>%
  select(region, years, pop_weighted_avg_pm2.5)

# create an identifier for South Asia and Middle East & North Africa 
ar_global_fig1.2_data <- gadm2_aqli_2022 %>%
  filter(!is.na(population)) %>%
  mutate(region = ifelse(country %in% south_asia_def, "South Asia", country), 
         region = ifelse(region %in% "China", "China", region), 
         region = ifelse(country %in% mena_countries, "Middle East & North Africa", region), 
         region = ifelse(region %in% c("China", "Middle East & North Africa", "South Asia"), region, "Rest of the World"))

# convert data from wide to long
ar_global_fig1.2_data <- ar_global_fig1.2_data %>%
  group_by(region) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2022_weighted, names_to = "years", 
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) 

# add global trendlines data to the above dataset
ar_global_fig1.2_data <- ar_global_fig1.2_data %>%
  rbind(trendlines_aqli_data_global)

ar_global_fig1.2_data$region <- factor(ar_global_fig1.2_data$region, levels = c("South Asia", "Middle East & North Africa", "China", "Rest of the World", "Global"))

ar_global_fig1.2 <- ar_global_fig1.2_data %>%
  ggplot() +  
  geom_line(mapping = aes(x = years, y = pop_weighted_avg_pm2.5, color = interaction(region), 
                          linetype = interaction(region), lwd = 0.7), lwd = 2) +
  labs(x = "Year", y = expression("Annual Average " ~ PM[2.5] ~ " concentrations (in µg/m³)"), 
       color = "") +
  ggthemes::theme_fivethirtyeight() +
  scale_color_manual(values = c("South Asia" = "#800026", 
                                "Middle East & North Africa" = "#8ea75b", 
                                "China" = "#D63333", 
                                "Rest of the World" = "#f29e37", 
                                "Global" = "#5e92a9"), 
                     breaks = c("South Asia", 
                                "Middle East & North Africa",
                                "China", 
                                "Rest of the World", 
                                "Global"), 
                     name = "legend") +
  scale_linetype_manual(values = c("South Asia" = "dashed",
                                   "Middle East & North Africa" = "dashed",
                                   "China" = "dashed", 
                                   "Rest of the World" = "dashed", 
                                   "Global" = "solid"), 
                        name = "legend") +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 7), 
        axis.title.y = element_text(size = 9), 
        axis.title.x = element_text(size = 9)) +
  scale_y_continuous(breaks = seq(0, 60, 10), limits = c(0, 60)) +
  scale_x_continuous(breaks = c(seq(1998, 2019, 3), 2022)) +
  guides(guide_legend(reverse = TRUE)) +
  themes_aqli_base +
  theme(plot.background = element_rect(fill = "white", color = "white"), 
        legend.title = element_blank(), 
        legend.key.width = unit(1, "cm"))