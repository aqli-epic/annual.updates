# read in the helper file
source("R/july.2025.helper.script.R")

# Fig 3: Potential gain in life expectancy in top 10 most populous states in Nigeria ------
nigeria_fs_fig3_data <- gadm1_aqli_2023 %>%
  filter(country == "Nigeria") %>%
  slice_max(population, n = 10) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2023")


nigeria_fs_fig3_data$lyl_bucket <- as.factor(nigeria_fs_fig3_data$lyl_bucket)

nigeria_fs_fig3 <- nigeria_fs_fig3_data %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(name_1, llpp_who_2023),
                         y = llpp_who_2023, fill = lyl_bucket),
           width = 0.5) +
  labs(x = "State", y = "Potential Gain in Life Expectancy (Years)",
       title = "",
       subtitle = "",
       caption = "",
       fill = "Potential gain in life expectancy (Years)") +
  scale_y_continuous(breaks = seq(0, 3, 1), limits = c(0, 3)) +
  scale_fill_manual(values = c("0 to < 0.1" = "#FFFFFF",
                               "0.1 to < 0.5" = "#FFF2E1",
                               "0.5 to < 1" = "#FFEDD3",
                               "1 to < 2" = "#FFC97A",
                               "2 to < 3" = "#FFA521",
                               "3 to < 4" = "#EB6C2A",
                               "4 to < 5" = "#D63333",
                               "5 to < 6" = "#8E2946",
                               ">= 6" = "#451F59")) +
  coord_flip() +
  ggthemes::theme_tufte() +
  themes_aqli_base +
  theme(axis.line = element_line(),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16, margin = margin(r = 0.7, unit = "cm")),
        axis.title.x = element_text(size = 16, margin = margin(t = 0.6, b = 0.6, unit = "cm")),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"))
