# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Chart number 10: Year over Year Change in PM2.5 Levels in 2020, the first year of the Pandemic

# Prepare  data set for chart number 10
chart10_dataset <- color_2020 %>%
  group_by(country) %>%
  mutate(pop_weights = population/sum(population, na.rm =TRUE),
         pm2019_pop_weighted = pop_weights*pm2019,
         pm2020_pop_weighted = pop_weights*pm2020) %>%
  summarise(avg_pm2.5_pop_weighted_2019 = sum(pm2019_pop_weighted, na.rm = TRUE),
            avg_pm2.5_pop_weighted_2020 = sum(pm2020_pop_weighted, na.rm = TRUE),
            change_pm2.5_2019_to_2020 = avg_pm2.5_pop_weighted_2020 - avg_pm2.5_pop_weighted_2019,
            percent_change_pm2.5_2019_to_2020 = (change_pm2.5_2019_to_2020/avg_pm2.5_pop_weighted_2019)*100,
            total_population = sum(population, na.rm = TRUE)) %>%
  mutate(direction_label = ifelse(change_pm2.5_2019_to_2020 < 0, "Negative", "Positive")) %>%
  slice_max(total_population, n = 20)

# Save AR chart number 10 dataset as a csv
chart10_dataset %>%
  write_csv("./june.2022/top.ten.charts.campaign/chart10.data.graph.code/chart10_dataset.csv")

# Plot chart number 10
chart10 <- chart10_dataset %>%
  ggplot(mapping = aes(x = fct_reorder(country, percent_change_pm2.5_2019_to_2020), y = percent_change_pm2.5_2019_to_2020, fill = direction_label)) + geom_bar(stat = "identity") +
  geom_hline(mapping = aes(yintercept = 0)) +
  scale_y_continuous(limits = c(-25, 25), breaks = seq(-25, 25, 5)) +
  labs(x = "country", y = "Year over Year change in PM2.5 (%)")+
  ggthemes::theme_clean() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "none")


# Save chart number 10
ggsave("./june.2022/top.ten.charts.campaign/chart10.data.graph.code/chart10.png", chart10)

