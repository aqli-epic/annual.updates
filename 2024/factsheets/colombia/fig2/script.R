# read in the helper file
source("R/july_2024_helper_script.R")

# Fig 2: Potential gain in life expectancy in states in colombia ------

# colombia fs fig 2 data
colombia_fs_fig2_dataset <- gadm2_aqli_2022 %>%
  filter(country == "Colombia") %>%
  select('country', 'name_1', 'name_2', 'population', 'pm2022', 'llpp_who_2022') %>%
  group_by(name_1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2022_pop_weighted = pop_weights*pm2022,
         llpp_who_2022_pop_weighted = pop_weights*llpp_who_2022) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), 
            avg_pm2.5_2022 = sum(pm2022_pop_weighted, na.rm = TRUE),
            le_gain = (avg_pm2.5_2022 - who_guideline)*le_constant,
            llpp_who_2022 = sum(llpp_who_2022_pop_weighted, na.rm = TRUE),
            le_gain = ifelse(le_gain < 0 , 0, le_gain)) %>%
  slice_max(tot_pop, n = 10) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2022")

# colombia fs figure 2
colombia_fs_fig2 <- colombia_fs_fig2_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(name_1, llpp_who_2022), y = llpp_who_2022, fill = lyl_bucket), width = 0.5) +
  labs(x = "Municipality", y = "Potential Gain in Life Expectancy (Years)", fill = "Potential gain in life expectancy (Years)") +
  scale_y_continuous(breaks = seq(0, 6, 1), limits = c(0, 2)) +
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
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 20, colour = "#222222"),
        legend.title = element_text(size = 20, colour = "#222222"),
        legend.box.background = element_rect(color = "black"),
        # plot.title = element_text(hjust = 0.5, size = 16), 
        # plot.subtitle = element_text(hjust =  0.5, size = 10, face = "italic", margin = margin(b = 0.8, unit = "cm")), 
        # plot.caption = element_text(size = 8, hjust = 0, face = "italic"),
        plot.background = element_rect(color = "white"),
        axis.line = element_line(), 
        axis.text = element_text(size = 20, colour = "#222222"), 
        axis.title.y = element_text(size = 24, colour = "#222222", margin = margin(r = 0.7, unit = "cm")), 
        axis.title.x = element_text(size = 24, colour = "#222222", margin = margin(t = 0.6, b = 0.6, unit = "cm")), 
        axis.ticks = element_blank()) 
