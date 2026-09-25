# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Chart number 7: Average PM2.5 Concentrations in India, 1998 to 2020

# filtering the color dataset for India
color_2020_india <- color_2020 %>%
  filter(country %in% "India")

# North India/Indo Gangetic Plain defintion
north_india <- c("West Bengal", "Uttar Pradesh", "Punjab", "Haryana",
                 "Chandigarh", "Bihar", "NCT of Delhi")

# adding a north India/rest of india region column (specifically Indo gangetic plain)
color_2020_india <- color_2020_india %>%
  mutate(region = ifelse(name_1 %in% north_india, "North India", "All other regions (excluding North India)"))

# chart #7 region wise (North India/All other regions excluding North India) 1998 to 2020 average PM2.5
chart7_region_wise_data <- color_2020_india %>%
  group_by(region) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2020_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  select(years, region, pop_weighted_avg_pm2.5)

# chart #7 (India National Average) 1998 to 2020 average PM2.5
chart_7_national_avg_1998_2020_tibble <- color_2020_india %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2020_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "National Average") %>%
  select(years, region, pop_weighted_avg_pm2.5)

# creating final chart7 dataset by combining the above 2 datasets: region wise and national average datasets
chart7_dataset <- rbind(chart7_region_wise_data, chart_7_national_avg_1998_2020_tibble)

# chart 7 plot
chart7 <- ggplot(chart7_dataset) +
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

# save chart7
ggsave("./june.2022/top.ten.charts.campaign/chart7.data.graph.code/chart7.png", chart7)

