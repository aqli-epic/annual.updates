# ------------------------------------------------------------------------------
# AQLI 2026 – China Top 5 Life Expectancy Threats
# Description : Identifies and visualizes the top 5 causes of life expectancy 
#               loss (LYL) in China using GBD data.
#
# Developed By: Purushottam Gupta
# Role        : Data Architect/Data Analyst, AQLI (University of Chicago)
# Contact     : guptap@uchicago.edu
# Dependencies: dplyr, readxl, ggplot2, forcats, ggthemes, stringr
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Load Dependencies & Data
# ------------------------------------------------------------------------------
source("~/R/helper_function_2026.R")
# ------------------------------------------------------------------------------
# Data Preparation: Filter + Top 5 Causes (China)
# ------------------------------------------------------------------------------
ar_china_fig8.2_dataset <- gbd_results_master_2026 %>%
  filter(cause_of_death %in% c("PM2.5 relative to WHO guideline", "Tobacco", "Dietary risks",                
                               "Transport injuries", "High alcohol use"), 
         country == "China")
# ------------------------------------------------------------------------------
# Add AQLI Color Buckets
# ------------------------------------------------------------------------------
colnames(ar_china_fig8.2_dataset)[3] <- c("llpp_who_2024")
ar_china_fig8.2_dataset <- ar_china_fig8.2_dataset %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2024")

# ------------------------------------------------------------------------------
# Visualization: Top 5 LYL Causes
# ------------------------------------------------------------------------------
ar_china_fig8.2_dataset$llpp_who_2024 <- as.numeric(ar_china_fig8.2_dataset$llpp_who_2024)
# plot 

ar_china_fig8.2 <- ar_china_fig8.2_dataset %>%
  ggplot() + 
  geom_col(mapping = aes(x = forcats::fct_reorder(cause_of_death, llpp_who_2024), 
                         y = llpp_who_2024, fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), 
           width = 0.6) +
  labs(x = "Threats to Life Expectancy", y = "Life Years Lost", fill = "Life years lost", 
       title = "") +
  #  coord_flip() + 
  ggthemes::theme_tufte() +
  themes_aqli_base +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 18),
        axis.title.y  = element_text(size = 18, margin = ggplot2::margin(r = 0.6, unit = "cm")),
        axis.title.x  = element_text(size = 18, margin = ggplot2::margin(t = 0.6, b = 0.6, unit = "cm")),
        plot.caption  = element_text(hjust = 0, size = 8, margin = ggplot2::margin(t = 0.8, unit = "cm")),
        plot.title    = element_text(hjust = 0.5, size = 20, margin = ggplot2::margin(b = 0.8, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 10, margin = ggplot2::margin(b = 0.8, unit = "cm")),
        legend.box.background = element_rect(color = "black"),
        axis.line = element_line(), 
        legend.text = element_text(size = 13), 
        legend.title = element_text(size = 14),
        panel.grid.major.y = element_blank(), 
        plot.background = element_rect(fill = "white", color = "white")) +
  scale_y_continuous(breaks = seq(0, 7, 1)) +
  
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
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
