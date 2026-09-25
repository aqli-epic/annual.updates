# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> India factsheet figure 2: Top 10 gadm level 1 most populous cities LE gains graph----------

# fig 2 dataset: PM2.5 in top 10 most populous states in 2020
india_fs_fig2_dataset <- color_2020_india %>%
  group_by(name_1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2020_pop_weighted = pop_weights*pm2020) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), avg_pm2.5_2020 = sum(pm2020_pop_weighted, na.rm = TRUE),
            le_gain = (avg_pm2.5_2020 - who_guideline)*le_constant,
            le_gain = ifelse(le_gain < 0 , 0, le_gain)) %>%
  slice_max(tot_pop, n = 10) %>%
  ungroup()

# Fig 2 plot
india_fs_fig2 <- india_fs_fig2_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(name_1, le_gain), y = le_gain, fill = avg_pm2.5_2020), width = 0.5) +
  labs(x = "State", y = "Average PM2.5 concentrations (in µg/m³)",
       subtitle = "Top 10 Most Polluted States in India") +
  scale_y_continuous(breaks = seq(0, 9, 1)) +
  scale_fill_gradient(low = "red", high = "darkred") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle("Average PM2.5 concentrations in 2020")

# save india fs figure 2
ggsave("./june.2022/factsheets/india/graphs/india_fs_fig2.png", india_fs_fig2)
