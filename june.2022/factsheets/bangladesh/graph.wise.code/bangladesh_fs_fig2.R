# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Bangladesh factsheet figure 2: LE gains if pollution is reduced to the WHO guideline in the Top 10 most populous regions----------

# filtering color dataset and keeping only bangladesh
color_2020_bangladesh <- color_2020 %>%
  filter(country == "Bangladesh")


# bangladesh fs figure 2 dataset
bangladesh_fs_fig2_dataset <- color_2020_bangladesh %>%
  group_by(name_1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2020_pop_weighted = pop_weights*pm2020) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), avg_pm2.5_2020 = sum(pm2020_pop_weighted, na.rm = TRUE),
            le_gain = (avg_pm2.5_2020 - who_guideline)*le_constant,
            le_gain = ifelse(le_gain < 0 , 0, le_gain)) %>%
  slice_max(tot_pop, n = 10) %>%
  ungroup()

# bangladesh fs figure 2
bangladesh_fs_fig2 <- bangladesh_fs_fig2_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(name_1, le_gain), y = le_gain, fill = le_gain), width = 0.5) +
  labs(x = "Region", y = "Average Life Expectancy Gains (Years)") +
  scale_y_continuous(breaks = seq(0, 12, 1)) +
  scale_fill_gradient(low = "red", high = "darkred") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none")

# save bangladesh factsheet figure 2
ggsave("./june.2022/factsheets/bangladesh/graphs/bangladesh_fs_figure2.png", bangladesh_fs_fig2)
