# Load in master helper file for june.2022 folder-------------------------------

source("./june.2022/R/june.2022.helper.script.R")

# AR figure 2 code--------------------------------------------------------------

#> Figure 2: Global Trends in PM2.5 Concentrations, 2000-2020

# calculating population weights and replacing the "pm" columns with their population weighted counterparts
color_2020_pop_weighted <- color_2020 %>%
  dplyr::mutate(global_pop_weights = population/sum(population)) %>%
  dplyr::mutate(across(starts_with("pm"), ~.x*global_pop_weights, .names = "{col}_weighted"))

# get year wise population weighted average in wide format
color_2020_summary <- color_2020_pop_weighted %>%
  dplyr::summarise(across(ends_with("weighted"), sum))

# convert the above data to long format
global_trends_figure_2020_dataset <- tibble(years = 1998:2020, global_avg_pm2.5_2020_dataset = as.numeric(unlist(color_2020_summary)))

# add le gains column
global_trends_figure_2020_dataset <- global_trends_figure_2020_dataset %>%
  dplyr::mutate(le_gains_red_pm2.5_to_who = round((global_avg_pm2.5_2020_dataset -  who_guideline)*le_constant, 1),
                le_gains_red_pm2.5_to_who = ifelse(le_gains_red_pm2.5_to_who < 0, 0, le_gains_red_pm2.5_to_who))

# filter data and only keep the following year range: 2000 to 2020
global_trends_figure_2020_dataset_filtered <- global_trends_figure_2020_dataset %>%
  dplyr::filter(years > 1999)

# plot the figure
ar_fig2 <- ggplot() +
  geom_line(global_trends_figure_2020_dataset_filtered, mapping = aes(x = years, y = global_avg_pm2.5_2020_dataset), color = "darkblue", lwd = 1.3) +
  theme_classic() +
  labs(x = "Years", y = expression(paste("Average PM2.5 concentration ( ", mu, "gm", "/", m^3, " )"))) +
  scale_x_continuous(breaks = seq(2000, 2020, 2)) +
  scale_y_continuous(breaks = seq(0, 35, 5), limits = c(0, 35)) +
  theme(legend.position = "none", legend.title = element_blank(), axis.title.x =  element_text(size = 8), axis.title.y = element_text(size = 10))

# save figure
ggsave("./june.2022/annual.report/figure2.data.graph/ar_fig2.png", ar_fig2)

# save figure data
write_csv(global_trends_figure_2020_dataset_filtered, "./june.2022/annual.report/figure2.data.graph/ar_fig2_data.csv")
