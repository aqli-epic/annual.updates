# read in the helper file
source("R/july.2024.helper.script.R")


# plot 4: Trendlines: 1998 to 2022------------------------------------

# bangladesh figure 4 data
bangladesh_fs_fig4_dataset <- gadm2_aqli_2022 %>%
  filter(country == "Bangladesh", !is.na(population)) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2022_weighted, names_to = "years", 
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))), 
         region = "National Average") %>% 
  select(years, region, pop_weighted_avg_pm2.5)

# bangladesh figure 4
  bangladesh_fs_fig4 <- bangladesh_fs_fig4_dataset %>%  
    ggplot() +
    geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), lwd = 1.1,   
            color = "#1a1638") +
    geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dashed") +
    geom_hline(mapping = aes(yintercept = 15), lwd = 0.8, linetype = "dashed") + 
    scale_y_continuous(breaks = seq(0, 80, 10), limits = c(0, 80)) +
    scale_x_continuous(breaks = c(seq(1998, 2019, 3), 2022)) +
    scale_color_manual(values = c("National Average" = "#1a1638")) +
    ggthemes::theme_clean() +
    themes_aqli_base +
    labs(x = "Year", 
       y = expression("Annual Average  " ~ PM[2.5] ~ " Concentration (in µg/m³)"), 
       title = "") +
    theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 7), 
        plot.background = element_rect(fill = "white", color = "white")) +  
    geom_text(x = 2001.8, y = 6.7, label =  expression("WHO " ~ PM[2.5] ~ " Guideline (last updated in 2022): 5 µg/m³"), size = 5) +
    geom_text(x = 2001.2, y = 16.7, label = expression("Bangladesh National" ~ PM[2.5] ~ "Standard: 15 µg/m³"), size = 5) 
  


