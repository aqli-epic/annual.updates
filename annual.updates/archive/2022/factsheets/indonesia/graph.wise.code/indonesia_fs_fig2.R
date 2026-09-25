# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Indonesia factsheet figure 2: LE gains if pollution is reduced to the WHO guideline in the Top 10 most populous regions----------

# fig2 dataset
indonesia_fs_fig2_dataset <-  color_2020_indonesia %>%
  mutate(name_1 = str_replace(name_1, "Jawa Barat", "West Java"),
         name_1 = str_replace(name_1, "Jawa Timur", "East Java"),
         name_1 = str_replace(name_1, "Jawa Tengah", "Central Java")) %>%
  group_by(name_1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2020_pop_weighted = pop_weights*pm2020) %>%
  summarise(avg_pm2.5_2020 = sum(pm2020_pop_weighted, na.rm = TRUE), gain_in_life_expectancy = (avg_pm2.5_2020 - who_guideline)*le_constant,
            gain_in_life_expectancy = ifelse(gain_in_life_expectancy < 0,  0, gain_in_life_expectancy),
            tot_pop = sum(population, na.rm = TRUE)) %>%
  slice_max(tot_pop, n = 10)

# fig2 plot

indonesia_fs_fig2 <- indonesia_fs_fig2_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(name_1, gain_in_life_expectancy), y = gain_in_life_expectancy, fill = gain_in_life_expectancy), width = 0.6) +
  labs(x = "Region", y = "Average Life Expectancy Gains (Years)") +
  scale_y_continuous(breaks = seq(0, 3, 0.5)) +
  scale_fill_gradient(low = "darkgoldenrod1", high = "darkorange") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none")

# save figure 2
ggsave("./june.2022/factsheets/indonesia/graphs/indonesia_fs_fig2.png", indonesia_fs_fig2)
