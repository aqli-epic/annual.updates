# read in the helper file
source("C:/Users/Aarsh/Downloads/annual.updates/R/july.2024.helper.script.R")

# southeast asia definition
se_asia_def <-  c("Brunei", "Myanmar", "Cambodia", "Timor-Leste", "Indonesia",
                  "Laos", "Malaysia", "Philippines", "Singapore", "Thailand",
                  "Vietnam")

# figure 3.2 ------------

# southeast asia figure 3.2 data
ar_se_asia_fig3.2_data <- gadm2_aqli_2022 %>%
  filter(country %in% se_asia_def) %>%
  select(country, name_1, name_2, population, pm2022, llpp_who_2022) %>%
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

# southeast asia figure 3.2: 10 most populous regions
ar_se_asia_fig3.2 <- ar_se_asia_fig3.2_data %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(name_1, llpp_who_2022), y = llpp_who_2022, fill = lyl_bucket), width = 0.5) +
  labs(x = "Region", y = "Potential Gain in Life Expectancy (Years)", fill = "Potential gain in life expectancy (Years)") +
  scale_y_continuous(breaks = seq(0, 3, 1), limits = c(0, 3)) +
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
  themes_aqli_base +
  theme(plot.background = element_rect(color = "white", fill = "white"))
ggsave("C:/Users/Aarsh/Downloads/ar_se_asia_fig3.2.png", ar_se_asia_fig3.2, width = 15, height = 10)




