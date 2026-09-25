# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Pakistan factsheet figure 2: Top 10 gadm level 1 most populous cities LE gains graph----------

# filtering color level data for Pakistan and adding a "region" column
color_2020_pak <- color_2020 %>%
  filter(country == "Pakistan") %>%
  mutate(region = ifelse(name_1  %in% c("Federal Capital Territory", "Punjab"),
                         name_1, "All other regions (excluding FCT and Punjab)"))


#> figure2: Top 10 gadm level 1 most populous cities LE gains graph
pak_fs_fig2_dataset <- color_2020_pak %>%
  group_by(name_1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2020_pop_weighted = pop_weights*pm2020) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), avg_pm2.5_2020 = sum(pm2020_pop_weighted, na.rm = TRUE),
            le_gain = (avg_pm2.5_2020 - who_guideline)*le_constant,
            le_gain = ifelse(le_gain < 0 , 0, le_gain)) %>%
  slice_max(tot_pop, n = 10) %>%
  ungroup()

# figure2 plot
pak_fs_fig2 <- pak_fs_fig2_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(name_1, le_gain), y = le_gain, fill = le_gain), width = 0.5) +
  labs(x = "Region", y = "Average Life Expectancy Gains (Years)") +
  scale_y_continuous(breaks = seq(0, 12, 1)) +
  coord_flip() +
  scale_fill_gradient(low = "orange", high = "red") +
  theme_classic() +
  theme(legend.position = "none")

# save figure 2
ggsave("./june.2022/factsheets/pakistan/graphs/pak_fs_fig_2.png", pak_fs_fig2)
