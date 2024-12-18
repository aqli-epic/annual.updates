# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> South East Asia factsheet figure 2: LE gains if pollution is reduced to the WHO guideline in the Top 10 largest regions of Southeast Asia

# seasia factsheet figure 2 dataset: filter dataset for South East Asia and add a new column that combines gadm_1 and gadm_0 columns
seasia_fs_fig2_dataset <- color_data_se_asia %>%
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

# seasia figure 2
seasia_fs_fig2 <- seasia_fs_fig2_dataset %>%
  ggplot(mapping = aes(x = forcats::fct_reorder(name_1_country, gain_in_le), y = gain_in_le, fill = gain_in_le)) +
  geom_col() +
  labs(y = "Gain In Life Expectancy (Years)", x = "Region") +
  coord_flip() +
  scale_fill_gradient(low = "darkgoldenrod1", high = "darkorange2") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none")

# save seasia figure 2
ggsave("./june.2022/factsheets/seasia/graphs/seasia_fs_fig2.png", seasia_fs_fig2)
