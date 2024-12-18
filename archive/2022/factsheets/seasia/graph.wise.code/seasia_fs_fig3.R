# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> South East Asia factsheet figure 3: Average PM2.5 concentrations in Southeast Asia, 1998 to 2020

#> seasia figure 3

# seasia figure 3 dataset
seasia_fs_fig3_dataset <- color_data_se_asia %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2020_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "Southeast Asia average") %>%
  select(years, region, pop_weighted_avg_pm2.5)

# seasia figure 3
seasia_fs_fig3 <- seasia_fs_fig3_dataset %>%
  ggplot() +
  geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), lwd = 1.1,
            color = "darkred") +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8) +
  scale_y_continuous(breaks = seq(0, 25, 2), limits = c(0, 25)) +
  scale_x_continuous(breaks = seq(1998, 2020, 2)) +
  ggthemes::theme_clean() +
  labs(x = "Years",
       y = expression(paste("Average PM2.5 concentration ( ", mu, "g", "/", m^3, " )"))) +
  theme(legend.position = "none", legend.title = element_blank(),
        legend.text = element_text(size = 7),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9)) +
  geom_text(x = 2002.8, y = 6.2, label = expression(paste("WHO PM2.5 Guideline (last updated: 2021): 5 ", mu, "g","/", m^3, "")))

# save seasia figure 3
ggsave("./june.2022/factsheets/seasia/graphs/seasia_fs_fig3.png", seasia_fs_fig3)
