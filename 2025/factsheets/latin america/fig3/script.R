# read in the helper file
source("~/R/july.2025.helper.script.R") 


# 10 most populated countries in Latin America (fig 3)--------------
# Latin America definition
latin_america_countries_vec <- c("México", "Guatemala", "Honduras", "El Salvador",
                                 "Nicaragua", "Costa Rica", "Panama", "Colombia",
                                 "Venezuela", "Ecuador", "Peru", "Bolivia",
                                 "Brazil", "Paraguay", "Chile", "Argentina",
                                 "Uruguay", "Cuba", "Haiti", "Dominican Republic",
                                 "Puerto Rico")

# FS Figure 3 dataset
latam_fs_fig3_dataset <- gadm2_aqli_2023 %>%
  filter(country %in% latin_america_countries_vec ) %>%
  select(country, name_1, name_2, population, pm2023, llpp_who_2023)

# Plot FS figure 3
latam_fs_fig3 <- latam_fs_fig3_dataset %>%
  group_by(country) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2023_pop_weighted = pop_weights*pm2023) %>%
  summarise(tot_poll = mean(pm2023, na.rm = TRUE), avg_pm2.5_pop_weighted = sum(pm2023_pop_weighted, na.rm = TRUE),
            gain_in_le = (avg_pm2.5_pop_weighted - who_guideline) * le_constant,
            gain_in_le = ifelse(gain_in_le < 0, 0, gain_in_le)) %>%
  ungroup() %>%
  slice_max(tot_poll, n = 10) %>%
  add_aqli_color_scale_buckets(scale_type = "lyl", col_name = "gain_in_le") %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(!!as.symbol("country"), !!as.symbol("gain_in_le")), y = !!as.symbol("gain_in_le"), fill = forcats::fct_reorder(!!as.symbol("lyl_bucket"), !!as.symbol("order_lyl_bucket")))) +
  scale_y_continuous(breaks = seq(0, 7, 1), limits = c(0, 3)) +
  scale_fill_manual(values = c(
    "0 to < 0.1" = "#FFFFFF", 
    "0.1 to < 0.5" = "#FFF2E1", 
    "0.5 to < 1" = "#FFEDD3", 
    "1 to < 2" = "#FFC97A", 
    "2 to < 3" = "#FFA521", 
    "3 to < 4" = "#EB6C2A", 
    "4 to < 5" = "#D63333", 
    "5 to < 6" = "#8E2946", 
    ">= 6" = "#451F59"))+
  labs(x = "Country", y = "Potential Gain in Life Expectancy (Years)", title = "", subtitle = "", caption = "", fill = "Potential gain in life expectancy (Years)") +
  ggthemes::theme_tufte() +
  theme(legend.position = "bottom",
        legend.justification = c(0.5, 3),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 24, color="#222222"),
        legend.title = element_text(size = 24, color="#222222"),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust =  0.5, size = 10, face = "italic", margin = margin(b = 0.8, unit = "cm")),
        plot.caption = element_text(size = 20, hjust = 0, face = "italic"),
        legend.box.background = element_rect(color = "black"),
        plot.background = element_rect(color = "white"),
        axis.line = element_line(),
        axis.text = element_text(size = 20, color="#222222"),
        axis.title = element_text(size = 24, color="#222222"),
        axis.title.y = element_text(margin = margin(r = 0.7, unit = "cm"), color="#222222"),
        axis.title.x = element_text(margin = margin(t = 0.6, b = 0.6, unit = "cm"), color="#222222"),
        axis.ticks = element_blank())+
  coord_flip()



