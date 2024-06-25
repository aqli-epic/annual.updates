# read in the helper file
# Inequality in AQ
ar_fig1.3_dataset <- data.frame(group = c("Least polluted  \n(Bottom 20 percentile)", 
                                          "Most polluted \n(Top 20 percentile)"),
                                PM2.5 = c(5.5, 33.9),
                                LYL = c(0.1, 2.8)) %>%
  add_aqli_color_scale_buckets("lyl", "LYL") %>%
  add_aqli_color_scale_buckets("pollution", "PM2.5")

pollution <- ar_fig1.3_dataset %>%
  ggplot(aes(x = group, y = PM2.5 , fill = forcats::fct_reorder(pol_bucket, order_pol_bucket))) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.5, colour = "black") +
  ylim(0, 35) +
  scale_fill_manual(values = c("0 to < 5" = "#a1f5ff", 
                               "5 to < 10" = "#92d4eb", 
                               "10 to < 20" = "#82b5d5", 
                               "20 to < 30" = "#7197be", 
                               "30 to < 40" = "#5f7aa5", 
                               "40 to < 50" = "#4e5e8b", 
                               "50 to < 60" = "#3c456f", 
                               "60 to < 70" = "#2b2d54", 
                               ">= 70" = "#1a1638")) +
  labs(x = "Pollution group", y = expression("Annual average " ~ PM[2.5] ~ "concentrations (in µg/m³)"),
       fill = "Annual average PM2.5 \nconcentrations (in µg/m³)") +
  coord_flip() +
  themes_aqli_base +
  theme(legend.position = "bottom", 
        axis.ticks = element_blank(), 
        strip.text = element_text(size = 14), 
        plot.background = element_rect(fill = "white", color = "white"))

lyl <- ar_fig1.3_dataset %>%
  ggplot(aes(x = group, y = LYL , fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket))) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.5, colour = "black") +
  ylim(0, 3) +
  scale_fill_manual(values = c("0 to < 0.1" = "#ffffff", 
                               "0.1 to < 0.5" = "#ffeda0", 
                               "0.5 to < 1" = "#fed976", 
                               "1 to < 2" = "#feb24c", 
                               "2 to < 3" = "#fd8d3c", 
                               "3 to < 4" = "#fc4e2a", 
                               "4 to < 5" = "#e31a1c", 
                               "5 to < 6" = "#bd0026", 
                               ">= 6" = "#800026")) +
  labs(x = "", y = "Potential gain in life expectancy (Years)",
       fill = "Potential gain in \nlife expectancy (Years)") +
  coord_flip() +
  themes_aqli_base +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom", 
        strip.text = element_text(size = 14), 
        plot.background = element_rect(fill = "white", color = "white"))

# Combine both graphs in a panel
ar_global_fig1.3 <- gridExtra::grid.arrange(pollution, lyl, nrow = 1, widths=c(3,2))
