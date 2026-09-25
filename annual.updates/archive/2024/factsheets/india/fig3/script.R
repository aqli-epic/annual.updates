# read in the helper file
source("R/july_2024_helper_script.R")

# Fig 3:  GBD India graph --------------------------------------------

# top 5 most deadly diseases
india_fs_fig3_dataset <- gbd_results_master_2022 %>%
  filter(country == "India") %>%
  slice_max(lyl, n = 5)

colnames(india_fs_fig3_dataset)[3] <- c("llpp_who_2022")

india_fs_fig3_dataset <- india_fs_fig3_dataset %>%
add_aqli_color_scale_buckets("lyl", "llpp_who_2022")

# figure 3 caption removed: 
# Source: Global Burden of Disease (https://vizhub.healthdata.org/gbd-results/) 
# causes and risks data and WHO Life Tables (https://apps.who.int/gho/data/node.main.LIFECOUNTRY?lang=en) 
# were used combined with the Life table method to arrive at these results. 
# 'PM2.5 relative to WHO Guideline' bar displays life years lost relative to the 
# WHO guideline as calculated by latest AQLI (2022) data, which will be published 
# in 2024.

india_fs_fig3 <- india_fs_fig3_dataset %>%
  ggplot() + 
  geom_col(mapping = aes(x = forcats::fct_reorder(cause_of_death, llpp_who_2022), 
                         y = llpp_who_2022, 
                         fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), 
           width = 0.4, color = "black") +
  labs(x = "Threats to Life Expectancy", y = "Life Years Lost", fill = "Life years lost", 
       title = expression("")) +
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
  scale_y_continuous(breaks = seq(0, 6, 0.5)) +
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



