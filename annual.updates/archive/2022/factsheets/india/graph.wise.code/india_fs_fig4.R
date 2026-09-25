# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> India factsheet figure 2: Average PM2.5 trend 1998 to 2020 in different regions of India (in comparison with the national average)----------

# creating India's region wise average PM2.5 data from 1998 to 2020
region_wise_avg_pm2.5_india_ts <- color_2020_india %>%
  group_by(region) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2020_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  select(years, region, pop_weighted_avg_pm2.5)

# creating a national average PM2.5 trendlines data from 1998 to 2020
national_avg_pm2.5_india_ts <- color_2020_india %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2020_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "National Average") %>%
  select(years, region, pop_weighted_avg_pm2.5)

# India factsheet figure 4 dataset
india_fs_fig4_dataset <- rbind(region_wise_avg_pm2.5_india_ts, national_avg_pm2.5_india_ts)

# India factsheet figure 4
india_fs_fig4 <- ggplot(india_fs_fig4_dataset) +
  geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5, color = region), lwd = 1.1) +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8) +
  scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(1998, 2020, 2)) +
  scale_color_manual(values = c("National Average" = "darkgrey", "North India" = "darkred", "All other regions (excluding North India)" = "orange")) +
  ggthemes::theme_clean() +
  labs(x = "Years",
       y = expression(paste("Average PM2.5 concentration ( ", mu, "g", "/", m^3, " )"))) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 6.5),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9)) +
  geom_text(x = 2002.8, y = 10, label = expression(paste("WHO PM2.5 Guideline (last updated: 2021): 5 ", mu, "g","/", m^3, "")))


# save india fs figure 4
ggsave("./june.2022/factsheets/india/graphs/india_fs_fig4.png", india_fs_fig4)

