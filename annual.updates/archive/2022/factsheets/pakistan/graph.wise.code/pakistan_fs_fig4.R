# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Pakistan factsheet figure 4: Average PM2.5, 1998 to 2020----------

# region wise pakistan data for 1998 to 2020
region_wise_pak_average_ts <- color_2020_pak %>%
  group_by(region) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2020_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  select(years, region, pop_weighted_avg_pm2.5)

# national average pakistan data for 1998 to 2020
national_average_pak_ts <- color_2020_pak %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2020_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "National Average") %>%
  select(years, region, pop_weighted_avg_pm2.5)

# pakistan factsheet figure 4 final dataset
pak_fs_fig4_dataset <- rbind(region_wise_pak_average_ts, national_average_pak_ts)

# pakistan factsheet figure 4
pak_fs_fig4 <- ggplot(pak_fs_fig4_dataset) +
  geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5, color = region), lwd = 1.1) +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8) +
  scale_y_continuous(breaks = seq(0, 60, 5), limits = c(0, 60)) +
  scale_x_continuous(breaks = seq(1998, 2020, 2)) +
  scale_color_manual(values = c("National Average" = "darkgrey", "Federal Capital Territory" = "darkred", "Punjab" = "orange", "All other regions (excluding FCT and Punjab)" = "tan")) +
  ggthemes::theme_clean() +
  labs(x = "Years",
       y = expression(paste("Average PM2.5 concentration ( ", mu, "g", "/", m^3, " )")),
       caption = "*FCT refers to the Federal Capital Territory region in Pakistan") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 7),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        plot.caption = element_text(hjust = 0, size = 5), plot.caption.position = "plot") +
  geom_text(x = 2002.7, y = 8, label = expression(paste("WHO PM2.5 Guideline (last updated: 2021): 5 ", mu, "g","/", m^3, "")))

# save pakistan factsheet figure 4
ggsave("./june.2022/factsheets/pakistan/graphs/pak_fs_fig_4.png", pak_fs_fig4)

