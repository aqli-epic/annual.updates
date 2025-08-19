# read in the helper file
source("R/july_2024_helper_script.R")

# Fig 3: GBD ------
# read and filter gbd results for colombia
diseases_list <- c("Tobacco",
                   "Child and maternal malnutrition",
                   "Self-harm and interpersonal violence",
                   "PM2.5 relative to WHO guideline")

colombia_fs_fig3_dataset <- gbd_results_master_2022  %>%
  filter(country == "Colombia", cause_of_death %in% diseases_list)

# filtering out those causes of death that are sort of* covered under PM2.5 in some broad way
colnames(colombia_fs_fig3_dataset)[3] <- c("llpp_who_2022")

colombia_fs_fig3_dataset <- colombia_fs_fig3_dataset %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2022")

# colombia_fs_fig3_dataset <- colombia_fs_fig3_dataset %>%
#   add_aqli_color_scale_buckets("lyl", "llpp_who_2022") %>%
#   slice_max(llpp_who_2022, n = 5)

# colombia factsheet figure 3
colombia_fs_fig3 <- colombia_fs_fig3_dataset %>%
  ggplot() + 
  geom_col(mapping = aes(x = reorder(cause_of_death, llpp_who_2022), y = llpp_who_2022, fill = reorder(lyl_bucket, order_lyl_bucket)), width = 0.5, color = "black") +
  labs(x = "Threats to life expectancy", y = "Life Years Lost", fill = "Life years lost") +
  coord_flip() + 
  ggthemes::theme_tufte() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 20, colour = "#222222"),
        legend.title = element_text(size = 20, colour = "#222222"),
        legend.box.background = element_rect(color = "black"),
        axis.text = element_text(size = 20, colour = "#222222"), 
        axis.title.y = element_text(size = 24, colour = "#222222", margin = margin(r = 0.6, unit = "cm")),
        axis.title.x = element_text(size = 24, colour = "#222222", margin = margin(t = 0.6, b = 0.6, unit = "cm")),
        axis.line = element_line(), 
        plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 0.8, unit = "cm"), face = "italic"),
        plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 0.8, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 0.8, unit = "cm"), face = "italic"),
        plot.background = element_rect(color = "white"),
        panel.grid.major.y = element_blank()) +
  scale_y_continuous(breaks = seq(0, 6, 0.5)) +
  # scale_x_discrete(limits = cause_of_death_ordered[seq(1, length(cause_of_death_ordered), by = 2)]) +
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

