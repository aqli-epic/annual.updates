# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Nigeria factsheet figure 2: LE gains if PM2.5 is reduced to the WHO guideline in top 10 most populous name_1 regions of Nigeria.

# No National Standard for Nigeria
nat_stan_nigeria <- "No National Standard"

# filtering the color file for Nigeria
color_2020_nigeria <- color_2020 %>%
  filter(country == "Nigeria") %>%
  mutate(region = ifelse(name_1 %in% c("Ekiti", "Lagos", "Ogun", "Ondo",
                                       "Osun", "Oyo"), "South-West Nigeria",
                         "All other regions (excluding South-West Nigeria)"))

# nigeria factsheet figure 2 dataset
nigeria_fs_fig2_dataset <-  color_2020_nigeria %>%
  group_by(name_1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2020_pop_weighted = pop_weights*pm2020) %>%
  summarise(avg_pm2.5_2020 = sum(pm2020_pop_weighted, na.rm = TRUE), gain_in_life_expectancy = (avg_pm2.5_2020 - who_guideline)*le_constant,
            gain_in_life_expectancy = ifelse(gain_in_life_expectancy < 0,  0, gain_in_life_expectancy),
            tot_pop = sum(population, na.rm = TRUE)) %>%
  slice_max(tot_pop, n = 10)

# nigeria fs figure 2
nigeria_fs_fig2 <- nigeria_fs_fig2_dataset %>% ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(name_1, gain_in_life_expectancy), y = gain_in_life_expectancy, fill = gain_in_life_expectancy)) +
  labs(x = "Region", y = "Average Life Expectancy Gains (Years)") +
  scale_y_continuous(breaks = seq(0, 3, 0.5)) +
  scale_fill_gradient(low = "burlywood1", high = "darkgoldenrod1") +
  coord_flip() +
  ggthemes::theme_clean() +
  theme(legend.position = "none")

# save nigeria factsheet figure 2
ggsave("./june.2022/factsheets/nigeria/graphs/nigeria_fs_fig2.png", nigeria_fs_fig2)
