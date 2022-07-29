# Load in master helper file for june.2022 folder-------------------------------

source("./june.2022/R/june.2022.helper.script.R")

# AR figure 7 code--------------------------------------------------------------

#> Figure 7: Year-Over-Year Change in PM2.5 Levels in 2020, the First Year of the Pandemic

# Prepare AR Figure 7 data set
ar_fig7_dataset <- color_2020 %>%
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

# Save AR figure 7 dataset as a csv
ar_fig7_dataset %>%
  write_csv("./june.2022/annual.report/figure7.data.graph/ar_fig7_dataset.csv")

# AR figure 7 plot
ar_fig7 <- ar_fig7_dataset %>%
  ggplot(mapping = aes(x = fct_reorder(country, percent_change_pm2.5_2019_to_2020), y = percent_change_pm2.5_2019_to_2020, fill = direction_label)) + geom_bar(stat = "identity") +
  geom_hline(mapping = aes(yintercept = 0)) +
  scale_y_continuous(limits = c(-25, 25), breaks = seq(-25, 25, 5)) +
  labs(x = "country", y = "Year over Year change in PM2.5 (%)")+
  ggthemes::theme_clean() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "none")


# Save AR figure 7
ggsave("./june.2022/annual.report/figure7.data.graph/ar_fig7.png", ar_fig7)
