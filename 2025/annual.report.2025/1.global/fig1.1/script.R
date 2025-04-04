# read in the helper file
source("~/R/july.2025.helper.script.R")

# Global section figure 1.1 ============
# create a version of the figure with the same diseases as used in the same figure in last year's report
ar_global_fig1.1_data <- gbd_results_master_2025 %>%
  filter(country == "Global", cause_of_death %in% c("PM2.5 relative to WHO guideline", 
                                                    "Tobacco", 
                                                    "Child and maternal malnutrition",  
                                                    "Alcohol use", 
                                                    "Transport injuries", 
                                                    "Unsafe water, sanitation, and handwashing", 
                                                    "Neglected tropical diseases and malaria", 
                                                    "HIV/AIDS and sexually transmitted infections", 
                                                    "Nutritional deficiencies"))
colnames(ar_global_fig1.1_data)[3] <- c("llpp_who_2023")

ar_global_fig1.1_data <- ar_global_fig1.1_data %>%
  mutate(lyl_bucket = ifelse((llpp_who_2023 >= 0) & (llpp_who_2023 < 0.1), "0 - < 0.1 years", NA), 
         lyl_bucket = ifelse((llpp_who_2023 >= 0.1) & (llpp_who_2023 <= 0.5), "0.1 - 0.5", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2023 > 0.5) & (llpp_who_2023 <= 1), "> 0.5 - 1", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2023 > 1) & (llpp_who_2023 <= 2), "> 1 - 2", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2023 > 2) & (llpp_who_2023 <= 3), "> 2 - 3", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2023 > 3) & (llpp_who_2023 <= 4), "> 3 - 4", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2023 > 4) & (llpp_who_2023 <= 5), "> 4 - 5", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2023 > 5) & (llpp_who_2023 < 6), "> 5 - < 6", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2023 >= 6), ">= 6", lyl_bucket)) %>%
  mutate(order_lyl_bucket = ifelse(lyl_bucket == "0 - < 0.1 years", 1, NA), 
         order_lyl_bucket = ifelse(lyl_bucket == "0.1 - 0.5", 2, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 0.5 - 1", 3, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 1 - 2", 4, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 2 - 3", 5, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 3 - 4", 6, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 4 - 5", 7, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 5 - 6", 8, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == ">= 6", 9, order_lyl_bucket))

# ar_fig1.1 plot
ar_global_fig1.1 <- ar_global_fig1.1_data %>%
  ggplot() + 
  geom_col(mapping = aes(x = forcats::fct_reorder(cause_of_death, llpp_who_2023), 
                         y = llpp_who_2023, fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), 
           width = 0.4, color = "black") +
  labs(x = "Threats to Life Expectancy", y = "Life years lost", fill = "Life years lost", 
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
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(), 
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 14),
        panel.grid.major.y = element_blank()) +
  scale_y_continuous(breaks = seq(0, 7, 1)) +
  scale_fill_manual(values = c("0 - < 0.1 years" = "#FFFFFF", 
                               "0.1 - 0.5" = "#FFE6B3", 
                               "> 0.5 - 1" = "#FFD25D", 
                               "> 1 - 2" = "#FFBA00", 
                               "> 2 - 3" = "#FF9600", 
                               "> 3 - 4" = "#FF6908", 
                               "> 4 - 5" = "#E63D23", 
                               "> 5 - < 6" = "#BD251C", 
                               ">= 6" = "#8C130E")) + 
  guides(fill = guide_legend(nrow = 1)) 