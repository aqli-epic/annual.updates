# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Indonesia factsheet figure 4: Indonesia average PM2.5 from 1998 to 2020, Indonesia

# indonesia factsheet figure 4 dataset: creating Indonesia's national average PM2.5 data from 1998 to 2020
indonesia_fs_fig4_dataset <- color_2020_indonesia %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2020_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "National Average") %>%
  select(years, region, pop_weighted_avg_pm2.5)

# indonesia factsheet figure 4
indonesia_fs_fig4 <- indonesia_fs_fig4_dataset %>%
  ggplot() +
  geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), lwd = 1.1,
            color = "darkred") +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30)) +
  scale_x_continuous(breaks = seq(1998, 2020, 2)) +
  scale_color_manual(values = c("National Average" = "darkgrey")) +
  ggthemes::theme_clean() +
  labs(x = "Years",
       y = expression(paste("Average PM2.5 concentration ( ", mu, "g", "/", m^3, " )"))) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 7),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9)) +
  geom_text(x = 2002.8, y = 6.3, label = expression(paste("WHO PM2.5 Guideline (last updated: 2021): 5 ", mu, "g","/", m^3, "")))

# save figure 4
ggsave("./june.2022/factsheets/indonesia/graphs/indonesia_fs_fig4.png", indonesia_fs_fig4)


