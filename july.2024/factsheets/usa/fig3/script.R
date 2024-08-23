# read in the helper file
source("R/july.2024.helper.script.R")


#Figure 3:  Change in life expectancy due to change in PM2.5 concentration in some of the most populous US states between 1970 and 2022-----------------

# calculate the x most populous states
x_most_populous_us_states <- gadm2_aqli_2022 %>%
  filter(country == "United States") %>%
  gadm_level_summary(c("country", "name_1"), c(2022), 10) %>%
  slice_max(population, n = 11) %>%
  select(name_1) %>%
  unlist() %>%
  as.vector()

# calculate pollution in the above states for the year 2022
us_fs_fig3_dataset_part1 <- gadm2_aqli_2022 %>%
  filter(country == "United States") %>%
  gadm_level_summary(c("country", "name_1"), c(2022), 10) %>%
  filter(name_1 %in% x_most_populous_us_states) %>%
  select(country, name_1, pm2022)

# calculate pollution in the above states for the year 1970
us_fs_fig3_dataset_part2 <- us_1970_calc_results_cleaned %>%
  #gadm_level_summary(c("country", "name_1"), c(1970), 10) %>%
  filter(name_1 %in% x_most_populous_us_states) %>%
  select(country, name_1, pm1970)

# final data set for figure 3
us_fs_fig3_final_data <- us_fs_fig3_dataset_part1 %>%
  left_join(us_fs_fig3_dataset_part2, by = c("country", "name_1")) %>%
  mutate(lyg_1970_2022= round((pm1970 - pm2022)*0.098, 2))
# %>%
#   add_aqli_color_scale_buckets(scale_type = "lyg_1970_2022")
mean_lyg <- us_fs_fig3_final_data %>%
  group_by(name_1) %>%
  summarize(mean_lyg = mean(lyg_1970_2022, na.rm = TRUE))

mean_lyg <- mean_lyg %>%
  mutate(lyg_category = cut(mean_lyg, breaks = c(0, 2, Inf),
                            labels = c("0.5 to (< 2)", ">= 2"), right = FALSE))
# figure 3
us_fs_fig3 <- mean_lyg %>%
  filter(name_1 != "Georgia") %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(!!as.symbol("name_1"), !!as.symbol("mean_lyg")), y = !!as.symbol("mean_lyg"), fill=lyg_category)) +
  labs(x = "State", y = "Life years gained", title = "", subtitle = "", caption = "", fill = "Life years gained") +
  themes_aqli_base +
  scale_fill_manual(values = c("0.5 to (< 2)" = "#74add1",
                               ">= 2" = "#4575b4")) +
  coord_flip() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 20, color="#222222"),
        axis.title.y = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"), color="#222222"),
        axis.title.x = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color="#222222"),
        plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 0.8, unit = "cm")),
        plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 0.8, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 0.8, unit = "cm")),
        legend.box.background = element_rect(color = "black"),
        axis.line = element_line(),
        legend.text = element_text(size = 20, color="#222222"),
        legend.title = element_text(size = 20, color="#222222"),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),axis.ticks.y = element_blank()) +
  scale_y_continuous(breaks = seq(0, 3, 0.5)) +
  guides(fill = guide_legend(nrow = 1))

