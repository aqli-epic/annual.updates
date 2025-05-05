# read in the helper file
source("~/R/july.2025.helper.script.R")

# Figure 2: Potential gain in life expectancy from reducing PM2.5 from 2023 levels to the WHO guideline in all provinces of Nepal ------

# nepal fs fig 2 data
nepal_fs_fig2_dataset <- gadm2_aqli_2023 %>%
  filter(country == "Nepal") %>%
  select('country', 'name_1', 'name_2', 'population', 'pm2023', 'llpp_who_2023') %>%
  group_by(name_1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2023_pop_weighted = pop_weights*pm2023,
         llpp_who_2023_pop_weighted = pop_weights*llpp_who_2023) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE),
            avg_pm2.5_2023 = sum(pm2023_pop_weighted, na.rm = TRUE),
            le_gain = (avg_pm2.5_2023 - who_guideline)*le_constant,
            llpp_who_2023 = sum(llpp_who_2023_pop_weighted, na.rm = TRUE),
            le_gain = ifelse(le_gain < 0 , 0, le_gain)) %>%
  slice_max(tot_pop, n = 10) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2023")

# nepal fs figure 2
nepal_fs_fig2 <- nepal_fs_fig2_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(name_1, llpp_who_2023), y = llpp_who_2023, fill = lyl_bucket), width = 0.5) +
  labs(x = "Province", y = "Potential Gain in Life Expectancy (Years)", fill = "Potential gain in life expectancy (Years)") +
  scale_y_continuous(breaks = seq(0, 6, 1), limits = c(0, 6)) +
  scale_fill_manual(values = c(
    "0 to < 0.1" = "#FFFFFF", 
    "0.1 to < 0.5" = "#FFF2E1", 
    "0.5 to < 1" = "#FFEDD3", 
    "1 to < 2" = "#FFC97A", 
    "2 to < 3" = "#FFA521", 
    "3 to < 4" = "#EB6C2A", 
    "4 to < 5" = "#D63333", 
    "5 to < 6" = "#8E2946", 
    ">= 6" = "#451F59")) +
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