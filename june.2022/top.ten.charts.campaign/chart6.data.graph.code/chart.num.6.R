# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Chart number 6: Countries with largest health burden due to air pollution and no National Ambient Air Quality Standards embedded in the Legislative Instruments.


#> Of the countries that don't have a NAAQS, what are the top 5 in terms of lifeyears lost.

# preparing the columns for the first 2 panels of the chart (population, LE gains)
ds_pop_le <- color_2020 %>%
  filter(country %notin% aaqs_places$Country) %>%
  group_by(country) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2020_pop_weighted = pm2020*pop_weights) %>%
  summarise(avg_pm2.5_2020 = sum(pm2020_pop_weighted, na.rm = TRUE),
            tot_pop = sum(population, na.rm = TRUE),
            tot_pop_millions = round(tot_pop/1000000, 1),
            avg_le_lost_in_years = round((avg_pm2.5_2020 - 5)*0.098, 1),
            avg_le_lost_in_years = ifelse(avg_le_lost_in_years < 0, 0, avg_le_lost_in_years))

# preparing the columns for the last panel of the figure (total person years lost)
ds_tot_lyl <- color_2020 %>%
  filter(country %notin% aaqs_places$Country) %>%
  mutate(avg_le_lost_in_years = (pm2020 - 5)*0.098,
         avg_le_lost_in_years = ifelse(avg_le_lost_in_years < 0, 0, avg_le_lost_in_years),
         le_lost_gadm2 = avg_le_lost_in_years*population) %>%
  group_by(country) %>%
  summarise(total_ly_lost = sum(le_lost_gadm2, na.rm = TRUE),
            total_ly_lost_millions = round(total_ly_lost/1000000, 1))


chart6_dataset <- ds_pop_le %>%
  left_join(ds_tot_lyl, by = "country")

chart6_dataset <- ds_3 %>%
  slice_max(total_ly_lost_millions, n = 5)


#> plot the above 3 columns in 3 separate plots and then combine into a single plot

# population plot
plt_1_pop <- chart6_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(country, total_ly_lost_millions), y = tot_pop_millions), fill = "darkred", width = 0.5) +
  ggthemes::theme_tufte() +
  labs(x = "Country", y = "Population (in millions)") +
  scale_y_continuous(breaks = seq(0, 120, 10)) +
  coord_flip()

# life expectancy plot
plt_2_le <- chart6_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(country, total_ly_lost_millions), y = avg_le_lost_in_years), fill = "darkred", width = 0.5) +
  ggthemes::theme_tufte() +
  labs(x = "", y = "Average Life Expectancy Lost (Years)") +
  scale_y_continuous(breaks = seq(0, 4, 0.5)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  coord_flip()

# total lifeyears lost plot
plt_3_tot_lyl <- chart6_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(country, total_ly_lost_millions), y = total_ly_lost_millions), fill = "darkred", width = 0.5) +
  ggthemes::theme_tufte() +
  labs(x = "", y = "Total Person Years Lost (in Millions)") +
  scale_y_continuous(breaks = seq(0, 300, 50)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  coord_flip()

# combine the above 3 plots into a single panel (1 x 3), i.e. 1 row 3 columns
chart6 <- gridExtra::grid.arrange(plt_1, plt_2, plt_3, nrow = 1)

