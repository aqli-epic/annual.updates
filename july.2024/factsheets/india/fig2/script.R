# read in the helper file
source("R/july_2024_helper_script.R")


# Fig 2: Potential gain in life expectancy in top 10 most populous states in India ----------

india_fs_fig2_data <- gadm_level_summary(gadm2_aqli_2022, c("country", "name_1"), c(2022),  10) %>%
  filter(country == "India") %>% 
  slice_max(population, n = 10) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2022")


india_fs_fig2_data$lyl_bucket <- as.factor(india_fs_fig2_data$lyl_bucket)

india_fs_fig2 <- india_fs_fig2_data %>%
  ggplot() +
   geom_col(mapping = aes(x = forcats::fct_reorder(name_1, llpp_who_2022), y = llpp_who_2022, fill = lyl_bucket), width = 0.5) +
  labs(x = "State", y = "Potential Gain in Life Expectancy (Years)",
       title = "",
       subtitle = "", 
       caption = "", 
       fill = "Potential gain in life expectancy (Years)") +
  scale_y_continuous(breaks = seq(0, 7, 1), limits = c(0, 7)) +
  scale_fill_manual(values = c("0 to < 0.1" = "#ffffff", 
                               "0.1 to < 0.5" = "#ffeda0", 
                               "0.5 to < 1" = "#fed976", 
                               "1 to < 2" = "#feb24c", 
                               "2 to < 3" = "#fd8d3c", 
                               "3 to < 4" = "#fc4e2a", 
                               "4 to < 5" = "#e31a1c", 
                               "5 to < 6" = "#bd0026", 
                               ">= 6" = "#800026")) +
  coord_flip() +
  ggthemes::theme_tufte() +
  themes_aqli_base +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust =  0.5, size = 10, face = "italic", margin = margin(b = 0.8, unit = "cm")), 
        plot.caption = element_text(size = 8, hjust = 0, face = "italic"),
        axis.line = element_line(), 
        axis.text = element_text(size = 11), 
        axis.title = element_text(size = 13), 
        axis.title.y = element_text(margin = margin(r = 0.7, unit = "cm")), 
        axis.title.x = element_text(margin = margin(t = 0.6, b = 0.6, unit = "cm")), 
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "white", color = "white")) 


