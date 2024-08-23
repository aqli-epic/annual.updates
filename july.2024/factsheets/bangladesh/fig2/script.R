# read in the helper file
source("R/july.2024.helper.script.R")
#' Fig 2: Potential Gain in Life Expectancy from Reducing PM2.5 from 2022 to
#' the WHO Guideline in 10 most populous regions of Bangladesh -----------------------------------------

# bangladesh figure 2 data
bangladesh_fs_fig2_dataset <- gadm2_aqli_2022 %>%
  filter(country == "Bangladesh") %>%
  add_aqli_color_scale_buckets(scale_type = "lyl", col_name = "llpp_who_2022") %>%
  slice_max(population, n = 10)
bangladesh_fs_fig2_dataset <- bangladesh_fs_fig2_dataset %>% mutate(dist_prov = str_c(name_2, " (", name_1, ")", sep = ""))
# bangladesh figure 2
bangladesh_fs_fig2 <- bangladesh_fs_fig2_dataset %>%
  aqli_bar(scale_type = "lyl", x_var = "dist_prov", y_var = "llpp_who_2022", title = "", subtitle = "", x_label = "District", y_label = "Potential Gain in Life Expectancy (Years)", legend_title = "Potential gain in life expectancy (Years)", caption = "") +
  themes_aqli_base +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 20,color = "#222222"),
        axis.title.y = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"),color = "#222222"),
        axis.title.x = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"),color = "#222222"),
        plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 0.8, unit = "cm")),
        plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 0.8, unit = "cm"), color = "#222222"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 0.8, unit = "cm"), color = "#222222"),
        legend.box.background = element_rect(color = "#222222"),
        axis.line = element_line(),
        legend.text = element_text(size = 20,color = "#222222"),
        legend.title = element_text(size = 20,color = "#222222"),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),axis.ticks.y = element_blank()) +
  scale_y_continuous(breaks = seq(0, 6, 2)) +
  guides(fill = guide_legend(nrow = 1))

