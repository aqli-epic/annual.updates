# Load in master helper file for june.2022 folder-------------------------------

source("./june.2022/R/june.2022.helper.script.R")

# AR figure 20 code--------------------------------------------------------------

#> Figure 20:  Global Population Weighted PM2.5 concentrations over time

# add population weighted pm column in the 2020 color dataset
color_2020_fig20 <- color_2020 %>%
  mutate(global_pop_weights = population/sum(population)) %>%
  mutate(across(starts_with("pm"), ~.x*global_pop_weights, .names = "{col}_weighted"))

# add population weighted pm column in the 2019 color dataset
color_2019_fig20 <- color_2019 %>%
  mutate(global_pop_weights = population/sum(population)) %>%
  mutate(across(starts_with("pm"), ~.x*global_pop_weights, .names = "{col}_weighted"))

# add population weighted pm column in the 2016 color dataset
color_2016_fig20 <- color_2016 %>%
  mutate(global_pop_weights = population/sum(population)) %>%
  mutate(across(starts_with("pm"), ~.x*global_pop_weights, .names = "{col}_weighted"))

# calculate average PM2.5 from population weighted column in the 2019 color dataset
color_2019_summary <- color_2019_fig20 %>%
  summarise(across(ends_with("weighted"), sum))

# calculate average PM2.5 from population weighted column in the 2020 color dataset
color_2020_summary <- color_2020_fig20 %>%
  summarise(across(ends_with("weighted"), sum))

# calculate average PM2.5 from population weighted column in the 2016 color dataset
color_2016_summary <- color_2016_fig20 %>%
  summarise(across(ends_with("weighted"), sum))

# Creating a list of datasets storing global trendlines data for each of color_2020, color_2019 and color_2016 datasets
global_trends_figure_list <- list(tibble(years = 1998:2019, global_avg_pm2.5 = as.numeric(unlist(color_2019_summary))), tibble(years = 1998:2020, global_avg_pm2.5 = as.numeric(unlist(color_2020_summary))), tibble(years = 2016:1998, global_avg_pm2.5 = as.numeric(unlist(color_2016_summary))))

# creating a final wide dataset from the individual datasets stored in the above list
final_dataset <- left_join(left_join(global_trends_figure_list[[2]], global_trends_figure_list[[1]], by = "years"), global_trends_figure_list[[3]], by = "years")

# renaming columns in the final wide dataset
final_dataset <- final_dataset %>%
  rename(`2020 dataset` = global_avg_pm2.5.x , `2019 dataset` = global_avg_pm2.5.y,
         `2016 dataset` = global_avg_pm2.5)

# converting dataset into a long format so that we can easily plot it using ggplot
final_dataset <- tidyr::pivot_longer(final_dataset, `2020 dataset`:`2016 dataset`,  names_to = "Reference Dataset", values_to = "PM2.5")

# adding a le_gains column
ar_fig20_dataset <- final_dataset %>%
  mutate(le_gains = (PM2.5 - 5)*0.098,
         le_gains = ifelse(le_gains < 0, 0, le_gains))

# Save AR Figure 20 data set as a csv
ar_fig20_dataset %>%
  write_csv("./june.2022/annual.report/figure15.data.graph/ar_fig20_dataset.csv")

# plotting Annual Report Figure 20
ar_fig20 <- ggplot(ar_fig20_dataset) +
  geom_line(mapping = aes(x = years, y = PM2.5, group = `Reference Dataset`, color = `Reference Dataset`), size = 1.75) +
  ggthemes::theme_stata() +
  labs(x = "Years", y = expression(paste("Average PM2.5 concentration ( ", mu, "g", "/", m^3, " )"))) +
  scale_x_continuous(breaks = seq(1998, 2020, 2)) +
  scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 40)) +
  scale_color_manual(values = c("lightgrey", "darkgrey", "darkred")) +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.title.x =  element_text(size = 8), axis.title.y = element_text(size = 10)) +
  ggtitle("Global Population Weighted PM2.5 Concentration Over Time")


# Save AR figure 20
ggsave("./june.2022/annual.report/figure20.data.graph/ar_fig20.png", ar_fig20)
