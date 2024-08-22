# read in the helper file
source("R/july.2024.helper.script.R")


# 15 most populated regions in Latin America (fig 5.3)--------------
# Latin America definition
latin_america_countries_vec <- c("México", "Guatemala", "Honduras", "El Salvador",
                                 "Nicaragua", "Costa Rica", "Panama", "Colombia",
                                 "Venezuela", "Ecuador", "Peru", "Bolivia",
                                 "Brazil", "Paraguay", "Chile", "Argentina",
                                 "Uruguay", "Cuba", "Haiti", "Dominican Republic",
                                 "Puerto Rico")

# AR Figure 5.3 dataset
ar_fig5.3_dataset <- gadm2_aqli_2022 %>%
  filter(country %in% latin_america_countries_vec ) %>%
  select(country, name_1, name_2, population, pm2022, llpp_who_2022)

ar_fig5.3_dataset <- ar_fig5.3_dataset %>% mutate(name_1_country = str_c(name_1, " (", country, ")", sep = ""))


# Plot AR figure 5.3
ar_fig5.3 <- ar_fig5.3_dataset %>%
  group_by(name_1_country) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2022_pop_weighted = pop_weights*pm2022) %>%
  summarise(tot_poll = mean(pm2022, na.rm = TRUE), avg_pm2.5_pop_weighted = sum(pm2022_pop_weighted, na.rm = TRUE),
            gain_in_le = (avg_pm2.5_pop_weighted - who_guideline) * le_constant,
            gain_in_le = ifelse(gain_in_le < 0, 0, gain_in_le)) %>%
  ungroup() %>%
  slice_max(tot_poll, n = 15) %>%
      add_aqli_color_scale_buckets(scale_type = "lyl", col_name = "gain_in_le") %>%
      ggplot() +
      geom_col(mapping = aes(x = forcats::fct_reorder(!!as.symbol("name_1_country"), desc(!!as.symbol("gain_in_le"))), y = !!as.symbol("gain_in_le"), fill = forcats::fct_reorder(!!as.symbol("lyl_bucket"), !!as.symbol("order_lyl_bucket"))), color = "black") +
      scale_fill_manual(values = c("0 to < 0.1" = "#ffffff",
                               "0.1 to < 0.5" = "#fff2e1",
                               "0.5 to < 1" = "#ffedd3",
                               "1 to < 2" = "#ffc97a",
                               "2 to < 3" = "#ffa521",
                               "3 to < 4" = "#eb6c2a",
                               "4 to < 5" =  "#d63333",
                               "5 to < 6" = "#8e2946",
                               ">= 6" = "#451f59"))+
      labs(x = "Region", y = "Potential Gain in Life Expectancy (Years)", title = "", subtitle = "", caption = "", fill = "Potential gain in life expectancy (Years)") +
      themes_aqli_base +
  theme(plot.background = element_rect(color = "white", fill = "white")) +
      coord_flip()
