# Load in master helper file for june.2022 folder-------------------------------

source("./june.2022/R/june.2022.helper.script.R")

# AR figure 13 code--------------------------------------------------------------

#> Figure 13: Potential Gain in Years of Life Expectancy Through Permanently Reducing PM2.5 from 2020 Concentrations to the WHO Guideline, in 15 Most Populated Regions in Latin America

# Latin America definition
latin_america_countries_vec <- c("Mexico", "Guatemala", "Honduras",
                                 "El Salvador", "Nicaragua",
                                 "Costa Rica", "Panama",
                                 "Colombia", "Venezuela",
                                 "Ecuador", "Peru",
                                 "Bolivia", "Brazil",
                                 "Paraguay", "Chile",
                                 "Argentina", "Uruguay",
                                 "Cuba", "Haiti", "Dominican Republic",
                                 "Puerto Rico")

# AR Figure 13 dataset
ar_fig13_dataset <- color_2020 %>%
  filter(country %in% latin_america_countries_vec) %>%
  mutate(name_1_country = str_c(name_1, "(", country, ")", sep = ""))

# Save AR Figure 13 dataset
ar_fig13_dataset %>%
  write_csv("./june.2022/annual.report/figure13.data.graph/ar_fig13_dataset.csv")


# Plot AR figure 13
ar_fig13 <- ar_fig13_dataset %>%
  group_by(name_1_country) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2020_pop_weighted = pop_weights*pm2020) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), avg_pm2.5_pop_weighted = sum(pm2020_pop_weighted, na.rm = TRUE),
            gain_in_le = (avg_pm2.5_pop_weighted - who_guideline) * le_constant,
            gain_in_le = ifelse(gain_in_le < 0, 0, gain_in_le)) %>%
  ungroup() %>%
  slice_max(tot_pop, n = 15) %>%
  ggplot(mapping = aes(x = forcats::fct_reorder(name_1_country, tot_pop), y = gain_in_le, fill = gain_in_le)) +
  geom_col() +
  labs(y = "Gain In Life Expectancy (Years)", x = "Region") +
  coord_flip() +
  ggthemes::theme_clean() +
  scale_fill_gradient(low = "lightgoldenrod", high = "darkorange2") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

# save annual report figure 13
ggsave("./june.2022/annual.report/figure13.data.graph/ar_fig13.png", ar_fig13)
