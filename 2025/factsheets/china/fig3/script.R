# read in the helper file
source("~/R/july.2025.helper.script.R")

# Figure 3: Potential gain in life expectancy from reducing PM2.5 concentrations from 2023 levels to the WHO guideline in the 10 most populous provinces of China

china_fs_fig3_dataset <- gadm1_aqli_2023 %>%
  filter(country == "China") %>%
  add_aqli_color_scale_buckets(scale_type = "lyl", col_name = "llpp_who_2023") %>%
  slice_max(population, n = 10)

china_fs_fig3 <- china_fs_fig3_dataset %>%
  aqli_bar(scale_type = "lyl", x_var = "name_1", y_var = "llpp_who_2023", title = "", subtitle = "", x_label = "Province", y_label = "Potential Gain in Life Expectancy (Years)", legend_title = "Potential gain in life expectancy (Years)", caption = "") +
  themes_aqli_base +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 20, color="#222222"),
        axis.title.y = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"), color="#222222"),
        axis.title.x = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color="#222222"),
        plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 0.8, unit = "cm")),
        plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 0.8, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 0.8, unit = "cm")),
        legend.box.background = element_rect(color = "black"),
        axis.line = element_line(),
        legend.text = element_text(size = 24, color="#222222"),
        legend.title = element_text(size = 24, color="#222222"),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),axis.ticks.y = element_blank()) +
  scale_y_continuous(breaks = seq(0, 7, 1), limits = c(0, 3)) +
  # scale_x_discrete(limits = cause_of_death_ordered[seq(1, length(cause_of_death_ordered), by = 2)]) +
  scale_fill_manual(values = c("0 to < 0.1" = "#FFFFFF", 
                               "0.1 to < 0.5" = "#FFF2E1", 
                               "0.5 to < 1" = "#FFEDD3", 
                               "1 to < 2" = "#FFC97A", 
                               "2 to < 3" = "#FFA521", 
                               "3 to < 4" = "#EB6C2A", 
                               "4 to < 5" = "#D63333", 
                               "5 to < 6" = "#8E2946", 
                               ">= 6" = "#451F59")) +
  guides(fill = guide_legend(nrow = 1))