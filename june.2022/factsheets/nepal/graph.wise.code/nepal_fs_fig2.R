# Load in master helper file for june.2022 folder-------------------------------

source("./june.2022/R/june.2022.helper.script.R")

#> Nepal Factsheet figure 2: LE gains if PM2.5 pollution is reduced to the WHO guideline, in top 10 most populous name_1 regions of Nepal

# filtering the color dataset for Nepal
color_2020_nepal <- color_2020 %>%
  filter(country == "Nepal")

# nepal factsheet figure 2 dataset
nepal_fs_fig2_dataset <- color_2020_nepal %>%
  group_by(name_1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2020_pop_weighted = pop_weights*pm2020) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), avg_pm2.5_2020 = sum(pm2020_pop_weighted, na.rm = TRUE),
            le_gain = (avg_pm2.5_2020 - who_guideline)*le_constant,
            le_gain = ifelse(le_gain < 0 , 0, le_gain)) %>%
  slice_max(tot_pop, n = 10) %>%
  ungroup()

# nepal fs figure 2
nepal_fs_fig2 <- nepal_fs_fig2_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(name_1, le_gain), y = le_gain, fill = le_gain), width = 0.5) +
  labs(x = "Region", y = "Average Life Expectancy Gains (Years)") +
  scale_y_continuous(breaks = seq(0, 12, 1)) +
  coord_flip() +
  scale_fill_gradient(low = "red", high = "darkred") +
  theme_classic() +
  theme(legend.position = "none")

# save nepal fs figure 2
ggsave("./june.2022/factsheets/nepal/graphs/nepal_fs_fig2.png", nepal_fs_fig2)

