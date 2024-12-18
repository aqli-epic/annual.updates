# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")

#> life years lost range graph------------------------------------

#  state wise min, average and max lyl dataset for 10 most populous states
lyl_range_df <- gadm2_aqli_2021 %>%
    filter(country == "Colombia") %>%
  group_by(name_1) %>%
    mutate(pop_weights = population/sum(population, na.rm = TRUE), 
           pm2021_pop_weighted = pm2021*pop_weights) %>%
  summarise(min_lyl = round(min(llpp_who_2021, na.rm = TRUE), 1),
            max_lyl = round(max(llpp_who_2021, na.rm = TRUE), 1),
            avg_pm2.5_2021 = sum(pm2021_pop_weighted, na.rm = TRUE),
            avg_lyl = round((avg_pm2.5_2021 - 5)*0.098, 1),
            range_lyl = max_lyl - min_lyl,
            total_population = sum(population, na.rm = TRUE)) %>%
    mutate(avg_lyl = ifelse(avg_lyl < 0, 0, avg_lyl))
  

# plot lyl range graph (click on zoom in the plots window in R-Studio to view the plot)
lyl_range_plt <- lyl_range_df  %>%
  ggplot(aes(x = forcats::fct_reorder(name_1, total_population), y = min_lyl)) +
  geom_point(aes(color = "Minimum life years lost"), size = 3) +
  geom_point(aes(y = max_lyl, color = "Maximum life years lost"), size = 3) +
  geom_point(aes(y = avg_lyl, color = "Average life years lost"), size = 3) +
  geom_segment(aes(xend = name_1, yend = max_lyl),
               linetype = "dashed", color = "black") +
  scale_color_manual(name = "", values = c("Minimum life years lost" = "cornflowerblue",
                                           "Average life years lost" = "darkgrey",
                                                   "Maximum life years lost" = "darkred"), 
                            breaks = c("Minimum life years lost", "Average life years lost", "Maximum life years lost"),
                     labels = c("Minimum life years lost", "Average life years lost", "Maximum life years lost")) +
  labs(title = expression("Minimum, Average and Maximum Life years lost to" ~ PM[2.5] ~ "Air Pollution"),
       x = "Department", y = "Life Years Lost") +
  coord_flip() +
  theme(legend.position = "bottom",
        legend.box.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 2, 0.5), limits = c(0, 2)) +
  ggthemes::theme_clean() +
    theme(legend.position = "bottom", 
        axis.title.y = element_text(margin = margin(r = 0.8, unit = "cm"), size = 16), 
        axis.title.x = element_text(margin = margin(t = 0.8, b = 1, unit = "cm"), size = 16), 
        axis.text = element_text(size = 14), 
        plot.title = element_text(hjust = 0.5, size = 18, margin = margin(b = 0.3, unit = "cm")),
        legend.box.background = element_rect(color = "black"),
        strip.text = element_text(size = 18, margin = margin(t = 1, b = 0.5, unit = "cm")),
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 0.8, unit = "cm"), face = "italic"),
        plot.caption = element_text(hjust = 0, size = 9, face = "italic", margin = margin(t = 1.3, unit = "cm")), 
        plot.background = element_rect(fill = "white", color = "white"))  




