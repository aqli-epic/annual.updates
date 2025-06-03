# read in the helper file
source("~/R/july.2025.helper.script.R") 

#Figure 4: Annual average PM2.5 concentrations in Cameroon, 1998-2023

cam_fs_fig4_dataset <- gadm2_aqli_2023 %>%
  filter(country == "Cameroon") %>%
  #filter(name_2 != "Tshumbe") %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2023_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "National Average") %>%
  select(years, region, pop_weighted_avg_pm2.5)

# CAM factsheet figure 4
cam_fs_fig4 <- cam_fs_fig4_dataset %>%
  ggplot() +
  geom_line(mapping = aes(x = as.integer(years),
                          y = as.double(pop_weighted_avg_pm2.5)),
            lwd = 1.1, color = "#7197be") +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 50)) +
  scale_x_continuous(breaks = c(seq(1998, 2020, 2), 2023))  +
  ggthemes::theme_tufte() +
  labs(x = "Year",
       y = expression("Annual Average" ~ PM[2.5] ~ "Concentration (in µg/m³)")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 24, color="#222222"),
        legend.title = element_blank(),
        axis.text = element_text(size = 20, color="#222222"),
        axis.title.y = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"), color="#222222"),
        axis.title.x = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color="#222222"),
        axis.line = element_line(),
        legend.box.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 0.7, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 8, face = "italic"),
        plot.caption = element_text(size = 7, margin = margin(t = 0.8, unit = "cm"), hjust = 0, face = "italic"),
        plot.background = element_rect(color = "white"),
        axis.ticks = element_blank()) +
  geom_text(x = 2003.6, y = 5.8, label = expression("WHO" ~ PM[2.5] ~ "Guideline (last updated: 2021): 5 µg/m³"), size = 7.5)