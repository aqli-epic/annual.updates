# Load in master helper file for june.2022 folder-------------------------------

source("./june.2022/R/june.2022.helper.script.R")

#> South Korea factsheet figure 2: LE gains if pollution is reduced to the   WHO guidelines in top 10 most populous name_1 regions in South Korea

# filter dataset for south korea
color_2020_south_korea <- color_2020 %>%
  filter(country %in% "South Korea")

# south korea factsheet figure 2 dataset
southkorea_fs_fig2_dataset <- color_2020_south_korea %>%
  group_by(name_1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2020_pop_weighted = pop_weights*pm2020) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), avg_pm2.5_2020 = sum(pm2020_pop_weighted, na.rm = TRUE),
            le_gain = (avg_pm2.5_2020 - who_guideline)*le_constant,
            le_gain = ifelse(le_gain < 0 , 0, le_gain)) %>%
  slice_max(tot_pop, n = 10) %>%
  ungroup()

# southkorea fs figure 2
southkorea_fs_fig2 <- southkorea_fs_fig2_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(name_1, le_gain), y = le_gain, fill = le_gain), width = 0.7) +
  labs(x = "Region", y = "Average Life Expectancy Gains (Years)") +
  scale_y_continuous(breaks = seq(0, 3, 0.5)) +
  scale_fill_gradient(low = "burlywood1", high = "darkgoldenrod1") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none")

# save southkorea factsheet figure2

ggsave("./june.2022/factsheets/southkorea/graphs/southkorea_fs_fig2.png", southkorea_fs_fig2)
