# read in the helper file
source("R/july_2024_helper_script.R")

# Fig 4: Average PM2.5 concentration in colombia from 1998 to 2022 ------
# colombia factsheet figure 4 dataset
colombia_fs_fig4_dataset <- gadm2_aqli_2022 %>%
  mutate(llpp_who_2022 = if_else(pm2022 <= whostandard, 0, llpp_who_2022)) %>%
  filter(country == "Colombia", !is.na(llpp_who_2022)) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2022_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "National Average") %>%
  select(years, region, pop_weighted_avg_pm2.5)

# colombia factsheet figure 4
colombia_fs_fig4 <- colombia_fs_fig4_dataset %>%
  ggplot() +
  geom_line(mapping = aes(x = as.integer(years), 
                          y = as.double(pop_weighted_avg_pm2.5)), 
            lwd = 1.1, color = "#3c456f") +
  # geom_smooth(aes(x = as.integer(years), y = as.double(pop_weighted_avg_pm2.5)),
  #             lwd = 0.5, linetype = "dashed", se = FALSE) +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dotted") +
  geom_hline(mapping = aes(yintercept = 20), lwd = 0.8, linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 60)) +
  scale_x_continuous(breaks = c(seq(1998, 2019, 3), 2022))  +
  ggthemes::theme_tufte() +
  labs(x = "Year", 
       y = expression("Annual Average" ~ PM[2.5] ~ "Concentration (in µg/m³)")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 20, colour = "#222222"),
        legend.title = element_text(size = 20, colour = "#222222"),
        legend.box.background = element_rect(color = "black"),
        axis.text = element_text(size = 20, colour = "#222222"), 
        axis.title.y = element_text(size = 24, colour = "#222222", margin = margin(r = 0.6, unit = "cm")), 
        axis.title.x = element_text(size = 24, colour = "#222222", margin = margin(t = 0.6, b = 0.6, unit = "cm")), 
        axis.line = element_line(), 
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 0.7, unit = "cm")), 
        plot.subtitle = element_text(hjust = 0.5, size = 8, face = "italic"), 
        plot.caption = element_text(size = 7, margin = margin(t = 0.8, unit = "cm"), hjust = 0, face = "italic"),
        plot.background = element_rect(color = "white")) +
  geom_text(x = 2003.8, y = 6.8, label = expression("WHO" ~ PM[2.5] ~ "Guideline (last updated: 2021): 5 µg/m³"), size = 7) +
  geom_text(x = 2001.8, y = 21.6, label = expression("National" ~ PM[2.5] ~ "Standard: 20 µg/m³"), size = 7)


