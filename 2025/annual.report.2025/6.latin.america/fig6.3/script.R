# read in the helper file
source("~/R/july.2025.helper.script.R") 


# 5 most populated countries in Latin America (fig 6.3)--------------
# Latin America definition
latin_america_countries_vec <- c("México", "Guatemala", "Honduras", "El Salvador",
                                 "Nicaragua", "Costa Rica", "Panama", "Colombia",
                                 "Venezuela", "Ecuador", "Peru", "Bolivia",
                                 "Brazil", "Paraguay", "Chile", "Argentina",
                                 "Uruguay", "Cuba", "Haiti", "Dominican Republic",
                                 "Puerto Rico")

# Latin America figure 6.3 ===========
# AR figure 6.3 dataset
ar_latam_fig6.3_dataset <- gadm2_aqli_2023 %>%
  filter(country %in% latin_america_countries_vec ) %>%
  select(country, name_1, name_2, population, pm2023, llpp_who_2023)

ar_latam_fig6.3_dataset <- ar_latam_fig6.3_dataset

# plot
ar_latam_fig6.3 <- ar_latam_fig6.3_dataset %>%
  group_by(country) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2023_pop_weighted = pop_weights*pm2023) %>%
  summarise(tot_poll = mean(pm2023, na.rm = TRUE), avg_pm2.5_pop_weighted = sum(pm2023_pop_weighted, na.rm = TRUE),
            gain_in_le = (avg_pm2.5_pop_weighted - who_guideline) * le_constant,
            gain_in_le = ifelse(gain_in_le < 0, 0, gain_in_le)) %>%
  ungroup() %>%
  slice_max(tot_poll, n = 5) %>%
  add_aqli_color_scale_buckets(scale_type = "lyl", col_name = "gain_in_le") %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(!!as.symbol("country"), !!as.symbol("gain_in_le")), y = !!as.symbol("gain_in_le"), fill = forcats::fct_reorder(!!as.symbol("lyl_bucket"), !!as.symbol("order_lyl_bucket"))), color = "black") +
  scale_fill_manual(values = c("0 to < 0.1" = "#FFFFFF", 
                               "0.1 to < 0.5" = "#FFF2E1", 
                               "0.5 to < 1" = "#FFEDD3", 
                               "1 to < 2" = "#FFC97A", 
                               "2 to < 3" = "#FFA521", 
                               "3 to < 4" = "#EB6C2A", 
                               "4 to < 5" = "#D63333", 
                               "5 to < 6" = "#8E2946", 
                               ">= 6" = "#451F59")) +
  labs(x = "Country", y = "Potential Gain in Life Expectancy (Years)", title = "", subtitle = "", caption = "", fill = "Potential gain in life expectancy (Years)") +
  themes_aqli_base +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 20, color="#222222"),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"), color="#222222"),
        axis.title.x = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color="#222222"),
        axis.line = element_line(),
        legend.box.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 0.7, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 8, face = "italic"),
        plot.caption = element_text(size = 7, margin = margin(t = 0.8, unit = "cm"), hjust = 0, face = "italic"),
        axis.text = element_text(size = 20, color="#222222"),
        plot.background = element_rect(color = "white", fill = "white")) +
  coord_flip()