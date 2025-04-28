# read in the helper file
source("R/july.2024.helper.script.R")

# Figure 3:  Change in life expectancy due to change in 10 most populous US states between 1998 and 2023 -------
# calculate the x most populous states
us_fs_fig3_data <- gadm1_aqli_2023 %>%
  filter(country == "United States") %>%
  slice_max(population, n = 10) %>%
  mutate(gain_lyl = llpp_who_1998 - llpp_who_2023) %>%
  add_aqli_color_scale_buckets("lyldiff", "gain_lyl")

# figure 3
us_fs_fig3 <- us_fs_fig3_data %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(name_1, gain_lyl),
                         y = gain_lyl,
                         fill=lyldiff_bucket)) +
  labs(x = "State", y = "Change in Life Expectancy (Years)",
       fill = "Change in life expectancy between 1998 and 2023 \n(Years; blue values indicate improvement)") +
  scale_fill_manual(values = c(">= 2" = "#008fbb",
                               "0.5 to (< 2)" = "#4fb6d3",
                               "0.1 to (< 0.5)" = "#99dbe9",
                               "0 to (< 0.1)" = "#d2eef4",
                               "-0.1 to (< 0)" = "#ffd393",
                               "-0.5 to (< -0.1)" = "#fea222",
                               "-2 to (< -0.5)" = "#ec6f29",
                               "< -2" = "#d63333")) +
  scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1)) +
  coord_flip() +
  themes_aqli_base +
  theme(axis.line = element_line(),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16, margin = margin(r = 0.7, unit = "cm")),
        axis.title.x = element_text(size = 16, margin = margin(t = 0.6, b = 0.6, unit = "cm")),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"))
