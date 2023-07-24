# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


# 15 most populated regions in Latin America (fig 5.1)--------------

# AR Figure 5.1 dataset
ar_fig5.1_dataset <- gadm2_aqli_2021_latin_america %>%
  mutate(name_1_country = str_c(name_1, "(", country, ")", sep = "")) 


# Plot AR figure 5.1
ar_fig5.1 <- ar_fig5.1_dataset %>%
  group_by(name_1_country) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         pm2021_pop_weighted = pop_weights*pm2021) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), avg_pm2.5_pop_weighted = sum(pm2021_pop_weighted, na.rm = TRUE), 
            gain_in_le = (avg_pm2.5_pop_weighted - who_guideline) * le_constant, 
            gain_in_le = ifelse(gain_in_le < 0, 0, gain_in_le)) %>%
  ungroup() %>%
  slice_max(tot_pop, n = 15) %>%
      add_aqli_color_scale_buckets(scale_type = "lyl", col_name = "gain_in_le") %>%
      ggplot() +
      geom_col(mapping = aes(x = forcats::fct_reorder(!!as.symbol("name_1_country"), !!as.symbol("gain_in_le")), y = !!as.symbol("gain_in_le"), fill = forcats::fct_reorder(!!as.symbol("lyl_bucket"), !!as.symbol("order_lyl_bucket"))), color = "black") +
      scale_fill_manual(values = c("0 to < 0.1" = "#ffffff",
                                   "0.1 to < 0.5" = "#ffeda0",
                                   "0.5 to < 1" = "#fed976",
                                   "1 to < 2" = "#feb24c",
                                   "2 to < 3" = "#fd8d3c",
                                   "3 to < 4" = "#fc4e2a",
                                   "4 to < 5" = "#e31a1c",
                                   "5 to < 6" = "#bd0026",
                                   ">= 6" = "#800026")) +
      labs(x = "Region", y = "Potential Gain in Life Expectancy (Years)", title = "", subtitle = "", caption = "", fill = "Potential gain in life expectancy (Years)") +
      themes_aqli_base +
  theme(plot.background = element_rect(color = "white", fill = "white")) +
      coord_flip()
    return(plt)




