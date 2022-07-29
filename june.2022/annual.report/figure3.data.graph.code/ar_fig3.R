# Load in master helper file for june.2022 folder-------------------------------

source("./june.2022/R/june.2022.helper.script.R")

# AR figure 3 code--------------------------------------------------------------

#> Figure 3: Potential Gain in Life Expectancy from Permanently Reducing PM2.5 from 2020 Concentrations to the WHO Guideline in the 10 Most Populated Countries in the World

# Creating the master dataset with all relevant columns to plot the 3 figures in the panel (will be arranged in a single row)
ar_fig3_dataset <- color_2020 %>%
  group_by(country) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2020_pop_weighted = pm2020*pop_weights,
         pm2019_pop_weighted = pm2019*pop_weights,
         le_gain_pre = ((pm2020 - who_guideline)*le_constant),
         le_gain_pre = ifelse(le_gain_pre < 0, 0, le_gain_pre),
         lifeyears_gained = le_gain_pre * population) %>%
  summarise(tot_population = sum(population, na.rm = TRUE),
            tot_population_millions = round((tot_population/1000000), 2),
            avg_pm2.5_2019 = sum(pm2019_pop_weighted, na.rm = TRUE),
            avg_pm2.5_2020 = sum(pm2020_pop_weighted, na.rm = TRUE),
            avg_life_exp_gain = round((avg_pm2.5_2020 - who_guideline)*le_constant, 2),
            avg_life_exp_gain = ifelse(avg_life_exp_gain < 0, 0, avg_life_exp_gain),
            total_person_years_gained = sum(lifeyears_gained, na.rm = TRUE),
            total_person_years_gained_billions = round(total_person_years_gained/1000000000, 2)) %>%
  ungroup() %>%
  slice_max(tot_population, n = 10)

# population plot of the top 10 most populous
plt_top10_most_populous_population <- ar_fig3_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(country, tot_population_millions), y = tot_population_millions), fill = "darkred", width = 0.7) +
  labs(x = "Country", y = "Population (in millions)") +
  scale_y_continuous(breaks = seq(0, 1500, 200)) +
  coord_flip() +
  ggthemes::theme_tufte()

# Average Life Expectancy Gains experienced by a person in top 10 most populous countries if PM2.5 is reduced to the WHO guideline
plt_top10_most_populous_le_gains_per_person <- ar_fig3_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(country, tot_population_millions), y = avg_life_exp_gain), fill = "darkred", width = 0.7) +
  labs(x = "", y = "Average Life Expectancy Gains (Years)") +
  scale_y_continuous(breaks = seq(0, 8, 1)) +
  ggthemes::theme_tufte() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  coord_flip()

# Total person years gained on the country level for the top 10 most populous regions in the world (if PM2.5 is reduced to the WHO guideline)

plt_top10_most_populous_tot_person_years_gained <- ar_fig3_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(country, tot_population_millions), y = total_person_years_gained_billions), fill = "darkred", width = 0.7) +
  labs(x = "", y = "Total Person Years Gained (Billion Years)") +
  scale_y_continuous(breaks = seq(0, 8, 1)) +
  ggthemes::theme_tufte() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  coord_flip()

# Combine all 3 graphs in a panel
ar_fig3 <- gridExtra::grid.arrange(plt_top10_most_populous_population , plt_top10_most_populous_le_gains_per_person, plt_top10_most_populous_tot_person_years_gained, nrow = 1)

# save the plot in the appropriate folder
ggsave("./june.2022/annual.report/figure3.data.graph/ar_fig3.png", ar_fig3)

# save the dataset that was reduced to generate the figure
write_csv(ar_fig3_dataset , "./june.2022/annual.report/figure3.data.graph/ar_fig3_data.csv")
