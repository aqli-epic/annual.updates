# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

# AR figure 9 code--------------------------------------------------------------

#> Figure 9: Potential Gain in Years of Life Expectancy Through Permanently Reducing PM2.5 from 2020 Concentrations to the WHO Guideline, in 10 Most Populated Regions in Southeast Asia

# South East Asia Definition
se_asia_vec <- c("Brunei", "Myanmar", "Cambodia", "Timor-Leste", "Indonesia", "Laos", "Malaysia", "Philippines", "Singapore", "Thailand", "Vietnam")

# filter dataset for South East Asia and add a new column that combines gadm_1 and gadm_0 columns
ar_fig9_dataset <- color_2020 %>%
  filter(country %in% se_asia_vec) %>%
  mutate(name_1_country = str_c(name_1, "(", country, ")", sep = "")) %>%
  group_by(name_1_country) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2020_pop_weighted = pop_weights*pm2020) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), avg_pm2.5_pop_weighted = sum(pm2020_pop_weighted, na.rm = TRUE),
            gain_in_le = (avg_pm2.5_pop_weighted - who_guideline) * le_constant,
            gain_in_le = ifelse(gain_in_le < 0, 0, gain_in_le)) %>%
  ungroup() %>%
  mutate(name_1_country = str_replace(name_1_country, "Jawa Barat", "West Java"),
         name_1_country = str_replace(name_1_country, "Jawa Timur", "East Java"),
         name_1_country = str_replace(name_1_country, "Jawa Tengah", "Central Java")
  ) %>%
  slice_max(tot_pop, n = 10)

# Save AR figure 9 dataset as a csv
ar_fig9_dataset %>%
  write_csv("./june.2022/annual.report/figure9.data.graph/ar_fig9_dataset.csv")

# AR figure 9
ar_fig9 <- ar_fig9_dataset %>%
  ggplot(mapping = aes(x = forcats::fct_reorder(name_1_country, gain_in_le), y = gain_in_le, fill = gain_in_le)) +
  geom_col() +
  labs(y = "Gain In Life Expectancy (Years)", x = "Region") +
  coord_flip() +
  scale_fill_gradient(low = "darkgoldenrod1", high = "darkorange2") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none")

# save AR figure 9
ggsave("./june.2022/annual.report/figure9.data.graph/ar_fig9.png", ar_fig9)
