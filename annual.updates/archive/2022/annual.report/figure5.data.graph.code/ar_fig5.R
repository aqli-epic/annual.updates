# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

# AR figure 5 code--------------------------------------------------------------

#> Figure 5: Global Trends in PM2.5 Concentrations, 2000-2020

# AR Figure 5

# South Asia definition
south_asia_def <- c("Afghanistan", "Bangladesh",
                    "Bhutan", "India",
                    "Maldives", "Nepal",
                    "Pakistan", "Sri Lanka")

# create an identifer for South Asia
color_2020 <- color_2020 %>%
  mutate(region = ifelse(country %in% south_asia_def, "South Asia", country),
         region = ifelse(region %in% "China", "China", region),
         region = ifelse(region %in% c("China", "South Asia"), region, "Rest of the World"))


# convert data from wode to long
color_2020_long <- color_2020 %>%
  group_by(region) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2020_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))))

# keep only years post 1999
color_2020_long_2000_2020 <- color_2020_long %>%
  filter(years > 1999)

# Save AR figure 5 dataset
color_2020_long_2000_2020 %>%
  write_csv("./june.2022/annual.report/figure5.data.graph/ar_fig5_data.csv")


# AR figure 5 plot
ar_fig5 <- ggplot(color_2020_long_2000_2020) +  geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5, color = region, lwd = 1.5), lwd = 2) +
  labs(x = "Years", y = "Average PM2.5 concentrations (in µg/m³)") +
  ggthemes::theme_fivethirtyeight() +
  scale_color_manual(values = c("chartreuse4", "blue4", "darkorange2")) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 7),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9)) +
  scale_y_continuous(breaks = seq(0, 60, 10)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1))

# save the plot in the appropriate folder
ggsave("./june.2022/annual.report/figure5.data.graph/ar_fig5.png", ar_fig5)
