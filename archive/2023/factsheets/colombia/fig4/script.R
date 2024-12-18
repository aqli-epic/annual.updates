# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


#> plot 4: Trendlines: 1998 to 2021------------------------------------

# Colombia factsheet figure 3 dataset
colombia_fs_fig4_dataset <- gadm2_aqli_2021 %>%
  filter(!is.na(population)) %>%
  filter(country == "Colombia") %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2021_weighted, names_to = "years", 
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))), 
         region = "National Average") %>% 
  select(years, region, pop_weighted_avg_pm2.5)



# plt
  colombia_fs_fig4 <- colombia_fs_fig4_dataset %>%
    ggplot() +
  geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), lwd = 1.1, 
            color = "#82b5d5") +
    geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dashed") +
    geom_hline(mapping = aes(yintercept = 20), lwd = 0.8, linetype = "dashed") + 
  scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 25)) +
  scale_x_continuous(breaks = c(seq(1998, 2019, 3), 2021)) +
    scale_color_manual(values = c("National Average" = "#82b5d5")) +
  ggthemes::theme_clean() +
    themes_aqli_base +
  labs(x = "Year", 
       y = expression("Annual Average  " ~ PM[2.5] ~ " Concentration (in µg/m³)"), 
       title = "") +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 7), 
        plot.background = element_rect(fill = "white", color = "white")) +  
    geom_text(x = 2001.5, y = 5.6, label =  expression("WHO " ~ PM[2.5] ~ " Guideline (last updated in 2021): 5 µg/m³"), size = 5) +
    geom_text(x = 2000.7, y = 20.7, label = expression("Colombia National" ~ PM[2.5] ~ "Standard: 20 µg/m³"), size = 5) 


