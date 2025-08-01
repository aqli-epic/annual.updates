# read in the helper file
source("~/R/july.2025.helper.script.R")

# Figure 2:Top 10 threats to life expectancy in Indonesia

# filtering out those causes of death that are sort of* covered under PM2.5 in some broad way
gbd_results_indonesia <- gbd_results_master_2025 %>%
  filter(cause_of_death %in% c("Ambient ozone pollution","Child and maternal malnutrition","Childhood sexual abuse and bullying",
                               "Dietary risks","Drug use","HIV/AIDS and sexually transmitted infections","High alcohol use","Intimate partner violence","Low physical activity",
                               "Neglected tropical diseases and malaria","Nutritional deficiencies","Occupational risks","PM2.5 relative to WHO guideline","Self-harm and interpersonal violence","Substance use disorders", 
                               "Tobacco","Transport injuries","Non-optimal temperature","Other environmental risks","Unintentional injuries","Unsafe sex","Unsafe water, sanitation, and handwashing"),country == "Indonesia")

colnames(gbd_results_indonesia)[3] <- c("llpp_who_2023")

indonesia_fs_fig2_dataset <- gbd_results_indonesia %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2023") %>%
  slice_max(llpp_who_2023, n = 10)

# indonesia factsheet figure 2
indonesia_fs_fig2 <- indonesia_fs_fig2_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(cause_of_death, llpp_who_2023), y = llpp_who_2023, fill = reorder(lyl_bucket, order_lyl_bucket)), width = 0.5) +
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
        legend.text = element_text(size = 24, color="#222222"),
        legend.title = element_text(size = 24, color="#222222"),
        panel.grid.major.y = element_blank()) +
  scale_y_continuous(breaks = seq(0, 7, 0.5), limits = c(0, 2)) +
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
