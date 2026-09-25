# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


# create a europe AQLI dataset
gadm2_aqli_2021_europe <- gadm2_aqli_2021 %>%
  filter(country %in% european_countries$Country)


# europe pop
europe_pop <- sum(gadm2_aqli_2021_europe$population, na.rm =  TRUE)

# pop living above who guideline
europe_above_who <- gadm2_aqli_2021_europe %>%
  filter(pm2021 > 10) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE))


# country wise average and how many are above WHO
europe_country_wise <- gadm2_aqli_2021_europe %>%
  gadm_level_summary(c("country"), c(2021), 10)


# exclude the following countries to keep the map less wide and to show a stark difference between eastern and western europe
exclude_countries <-  c("Russia", "Turkey", "Sweden", "Finland", "Norway", "Kazakhstan", "Iceland", "Georgia", "Azerbaijan", "Armenia", "Cyprus", "Northern Cyprus", "Svalbard and Jan Mayen")


countries_except_excluded <- european_countries %>% 
  filter(Country %notin% exclude_countries)




# creating region wise average PM2.5 data from 1998 to 2021 
europe_fs_fig4_data <- gadm2_aqli_2021_europe %>%
  filter(!is.na(population)) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2021_weighted, names_to = "years", 
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  select(years, pop_weighted_avg_pm2.5)


# fig 4
europe_fs_fig4 <- europe_fs_fig4_data %>%
  ggplot() +
  geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), color =  "#82b5d5", lwd = 1.3) +
    geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 25, 5), limits = c(0, 25)) +
  scale_x_continuous(breaks = c(seq(1998, 2019, 3), 2021)) +
  ggthemes::theme_tufte() +
  labs(x = "Year", 
       y = expression("Annual Average   " ~ PM[2.5] ~ " Concentration (in µg/m³)"), 
       title = "") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 13, margin = margin(r = 0.6, unit = "cm")), 
        axis.title.x = element_text(size = 13, margin = margin(t = 0.6, b = 0.6, unit = "cm")), 
        axis.line = element_line(), 
        legend.box.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 0.7, unit = "cm")), 
        plot.subtitle = element_text(hjust = 0.5, size = 8, face = "italic"), 
        plot.caption = element_text(size = 7, margin = margin(t = 0.8, unit = "cm"), hjust = 0, face = "italic"),
        axis.text = element_text(size = 12), 
        plot.background = element_rect(color = "white", fill = "white"), 
        axis.ticks = element_blank()) +
     geom_text(x = 2000.5, y = 5.7, label = expression("WHO" ~ PM[2.5] ~ "Guideline (last updated: 2021): 5 µg/m³"), size = 4.5) 


