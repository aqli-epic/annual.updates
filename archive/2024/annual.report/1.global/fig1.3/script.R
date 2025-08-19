# read in the helper file
source("R/july.2024.helper.script.R")

# Global section figure 1.3 ============
# Inequality in AQ
quantile(gadm2_aqli_2022$pm2022, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)

ar_fig1.3_dataset <- gadm2_aqli_2022 %>%
  select(country, name_1, name_2, population, pm2022) %>%
  filter(!is.na(pm2022)) %>%
  mutate(quintile = if_else(pm2022 >= 0 & pm2022 < 6.98, "Least polluted", NA),
         quintile = if_else(pm2022 >= 6.98 & pm2022 < 9.94, "2nd quintile", quintile),
         quintile = if_else(pm2022 >= 9.94 & pm2022 < 13.47, "3rd quintile", quintile),
         quintile = if_else(pm2022 >= 13.47 & pm2022 < 18.75, "4th  quintile", quintile),
         quintile = if_else(pm2022 >= 18.75, "Most polluted", quintile)) %>%
  group_by(quintile) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  mutate(LYL = round(0.098*(pm2022_weighted-5), 2)) %>%
  add_aqli_color_scale_buckets("lyl", "LYL")

ar_fig1.3_dataset$quintile <- factor(ar_fig1.3_dataset$quintile, levels = c("Least polluted", 
                                                                            "2nd quintile", 
                                                                            "3rd quintile", 
                                                                            "4th  quintile", 
                                                                            "Most polluted"))

ar_global_fig1.3 <- ar_fig1.3_dataset %>%
  ggplot(aes(x = quintile, y = LYL , fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket))) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.5, colour = "black") +
  ylim(0, 3) +
  scale_fill_manual(values = c("0 to < 0.1" = "#FFFFFF", 
                               "0.1 to < 0.5" = "#FFF2E1", 
                               "0.5 to < 1" = "#FFEDD3", 
                               "1 to < 2" = "#FFC97A", 
                               "2 to < 3" = "#FFA521", 
                               "3 to < 4" = "#EB6C2A", 
                               "4 to < 5" = "#D63333", 
                               "5 to < 6" = "#8E2946", 
                               ">= 6" = "#451F59")) +
  labs(x = "Pollution quintile", y = "Potential gain in life expectancy (Years)",
       fill = "Potential gain in \nlife expectancy (Years)") +
  themes_aqli_base +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom", 
        strip.text = element_text(size = 14), 
        plot.background = element_rect(fill = "white", color = "white"))
