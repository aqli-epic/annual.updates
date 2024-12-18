# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Central and West Africa factsheet figure 3: · Potential Gain in Years of Life Expectancy Through Permanently Reducing
# PM2.5 from 2020 Concentrations to the WHO  Guideline, in 10 Largest Regions in Central and West Africa

# central and west africa figure 3 dataset
cwafrica_fs_fig3_dataset <- color_data_cen_west_africa %>%
  group_by(name_1_country) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2020_pop_weighted = pop_weights*pm2020) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), avg_pm2.5_pop_weighted = sum(pm2020_pop_weighted, na.rm = TRUE),
            gain_in_le = (avg_pm2.5_pop_weighted - who_guideline) * 0.098,
            gain_in_le = ifelse(gain_in_le < 0, 0, gain_in_le)) %>%
  ungroup() %>%
  slice_max(tot_pop, n = 15)

# central and west africa figure 3
cwafrica_fs_fig3 <- cwafrica_fs_fig3_dataset %>%
  ggplot(mapping = aes(x = forcats::fct_reorder(name_1_country, avg_pm2.5_pop_weighted), y = gain_in_le, fill = gain_in_le)) +
  geom_col(width = 0.5) +
  labs(y = "Gain In Life Expectancy (Years)", x = "Region") +
  coord_flip() +
  ggthemes::theme_clean() +
  scale_y_continuous(breaks = seq(0, 4, 1), limits = c(0, 4)) +
  scale_fill_gradient(low = "orange", high = "red") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 9))

# save cwafrica fig3
ggsave("./june.2022/factsheets/cwafrica/graphs/cwafrica_fig3.png", cwafrica_fs_fig3)

