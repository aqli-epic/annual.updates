# read in the helper file
source("R/july.2024.helper.script.R")

# figure 6.4 ============

# top 5 most deadly diseases
china_fs_fig6.4_dataset <- gbd_results_master_2022 %>%
  filter(country == "China") %>%
  slice_max(lyl, n = 5)

colnames(china_fs_fig6.4_dataset)[3] <- c("llpp_who_2022")

china_fs_fig6.4_dataset <- china_fs_fig6.4_dataset %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2022")

# figure 4 plot 
china_fs_fig6.4 <- china_fs_fig6.4_dataset %>%
  ggplot() + 
  geom_col(mapping = aes(x = forcats::fct_reorder(cause_of_death, llpp_who_2022), 
                         y = llpp_who_2022, fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), 
           width = 0.4, color = "black") +
  labs(x = "Threats to Life Expectancy", y = "Life Years Lost", fill = "Life years lost", 
       title = "") +
  coord_flip() + 
  ggthemes::theme_tufte() +
  themes_aqli_base +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16, margin = margin(r = 0.6, unit = "cm")),
        axis.title.x = element_text(size = 16, margin = margin(t = 0.6, b = 0.6, unit = "cm")),
        plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 0.8, unit = "cm")),
        plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 0.8, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 0.8, unit = "cm")),
        legend.box.background = element_rect(color = "black"),
        axis.line = element_line(), 
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 14),
        panel.grid.major.y = element_blank(), 
        plot.background = element_rect(fill = "white", color = "white")) +
  scale_y_continuous(breaks = seq(0, 7, 1)) +
  scale_fill_manual(values = c("0 to < 0.1" = "#ffffff", 
                               "0.1 to < 0.5" = "#ffeda0", 
                               "0.5 to < 1" = "#fed976", 
                               "1 to < 2" = "#feb24c", 
                               "2 to < 3" = "#fd8d3c", 
                               "3 to < 4" = "#fc4e2a", 
                               "4 to < 5" = "#e31a1c", 
                               "5 to < 6" = "#bd0026", 
                               ">= 6" = "#800026")) + 
  guides(fill = guide_legend(nrow = 1)) 
