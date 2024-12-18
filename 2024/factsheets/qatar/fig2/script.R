# read in the helper file
source("R/july.2024.helper.script.R")

#Figure 2:Top 10 threats to life expectancy in Qatar

# filtering out those causes of death that are sort of* covered under PM2.5 in some broad way
gbd_results_Qatar <- gbd_results_master_2022 %>%
  filter(cause_of_death!= "Cardiovascular diseases")%>%
  filter(cause_of_death!= "Respiratory infections and tuberculosis")%>%
  filter(cause_of_death!= "Chronic respiratory diseases")%>%
  filter(cause_of_death!= "Non-optimal temperature")%>%
  filter(country == "Qatar")

colnames(gbd_results_Qatar)[3] <- c("llpp_who_2022")

Qatar_fs_fig2_dataset <- gbd_results_Qatar %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2022") %>%
  slice_max(llpp_who_2022, n = 10)

# Qatar factsheet figure 2
Qatar_fs_fig2 <- Qatar_fs_fig2_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(cause_of_death, llpp_who_2022), y = llpp_who_2022, fill = reorder(lyl_bucket, order_lyl_bucket)), width = 0.5, color = "black") +
  labs(x = "Threats to life expectancy", y = "Life Years Lost", fill = "Life years lost") +
  coord_flip() +
  ggthemes::theme_tufte() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 20, color="#222222"),
        axis.title.y = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"), color="#222222"),
        axis.title.x = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color="#222222"),
        plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 0.8, unit = "cm"), face = "italic"),
        plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 0.8, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 0.8, unit = "cm"), face = "italic"),
        legend.box.background = element_rect(color = "black"),
        plot.background = element_rect(color = "white"),axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(size = 20, color="#222222"),
        legend.title = element_text(size = 20, color="#222222"),
        panel.grid.major.y = element_blank()) +
  scale_y_continuous(breaks = seq(0, 7, 0.5)) +
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
