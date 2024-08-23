# read in the helper file
source("C:/Users/Aarsh/Downloads/annual.updates/R/july.2024.helper.script.R")

# Figure 2: Potential gain in life expectancy from reducing PM2.5 from 2022 levels to the WHO guideline in all provinces of Nepal ------

# nepal fs fig 2 data
nepal_fs_fig2_dataset <- gadm2_aqli_2022 %>%
  filter(country == "Nepal") %>%
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

# nepal fs figure 2
nepal_fs_fig2 <- nepal_fs_fig2_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(name_1, llpp_who_2022), y = llpp_who_2022, fill = lyl_bucket), width = 0.5) +
  labs(x = "Province", y = "Potential Gain in Life Expectancy (Years)", fill = "Potential gain in life expectancy (Years)") +
  scale_y_continuous(breaks = seq(0, 6, 1), limits = c(0, 6)) +
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
        legend.text = element_text(size = 20, color="#222222"),
        legend.title = element_text(size = 20, color="#222222"),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust =  0.5, size = 10, face = "italic", margin = margin(b = 0.8, unit = "cm")),
        plot.caption = element_text(size = 8, hjust = 0, face = "italic"),
        legend.box.background = element_rect(color = "black"),
        plot.background = element_rect(color = "white"),
        axis.line = element_line(),
        axis.text = element_text(size = 20, color="#222222"),
        axis.title = element_text(size = 24, color="#222222"),
        axis.title.y = element_text(margin = margin(r = 0.7, unit = "cm")),
        axis.title.x = element_text(margin = margin(t = 0.6, b = 0.6, unit = "cm")),
        axis.ticks = element_blank())
ggsave("C:/Users/Aarsh/Downloads/nepal_fs_fig2.png", nepal_fs_fig2, width = 15, height = 10)
svglite("nepal_fs_fig2")
ggsave("C:/Users/Aarsh/Downloads/nepal_fs_fig2.svg", width = 15, height = 10)
