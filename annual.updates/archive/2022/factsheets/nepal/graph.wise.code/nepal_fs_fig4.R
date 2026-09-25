# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Nepal factsheet figure 4: Average PM2.5 concentrations in Nepal, from 1998 to 2020------------

# filtering the color dataset for Nepal
color_2020_nepal <- color_2020 %>%
  filter(country == "Nepal")

# nepal factsheet figure 4 dataset
nepal_fs_fig4_dataset <- color_2020_nepal %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2020_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "National Average") %>%
  select(years, region, pop_weighted_avg_pm2.5)


# nepal factsheet figure 4
nepal_fs_fig4 <- nepal_fs_fig4_dataset %>%
  ggplot() +
  geom_line(mapping = ggplot2::aes(x = as.integer(years), y = as.double(pop_weighted_avg_pm2.5)), lwd = 1.1,
            color = "darkred") +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8) +
  scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 60)) +
  scale_x_continuous(breaks = seq(1998, 2020, 2))  +
  ggthemes::theme_clean() +
  labs(x = "Years",
       y = expression(paste("Average PM2.5 concentration ( ", mu, "g", "/", m^3, " )"))) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 7),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9)) +
  geom_text(x = 2003.5, y = 8.7, label = expression(paste("WHO PM2.5 Guideline (last updated: 2021): 5 ", mu, "g","/", m^3, "")))

# save nepal fs figure 4
ggsave("./june.2022/factsheets/nepal/graphs/nepal_fs_fig4.png", nepal_fs_fig4)
