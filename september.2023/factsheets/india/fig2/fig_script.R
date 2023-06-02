
# read in the helper file (look for appPublic folder in the root of the repository)
source("./appPublic/aqli.data.explorer.helper.script.R")

# india fs fig2 data
india_fs_fig2_data <- gadm_level_summary(gadm2_aqli_2021, c("country", "name_1"), c(2021),  10) %>%
  filter(country == "India") %>% 
  slice_max(population, n = 10) %>%
  mutate(lyl_bucket = ifelse((llpp_who_2021 >= 0) & (llpp_who_2021 < 0.1), "0 - < 0.1 years", NA), 
         lyl_bucket = ifelse((llpp_who_2021 >= 0.1) & (llpp_who_2021 <= 0.5), "0.1 - 0.5", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 0.5) & (llpp_who_2021 <= 1), "> 0.5 - 1", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 1) & (llpp_who_2021 <= 2), "> 1 - 2", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 2) & (llpp_who_2021 <= 3), "> 2 - 3", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 3) & (llpp_who_2021 <= 4), "> 3 - 4", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 4) & (llpp_who_2021 <= 5), "> 4 - 5", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 5) & (llpp_who_2021 < 6), "> 5 - < 6", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 >= 6), ">= 6 years", lyl_bucket)) %>%
  mutate(order_lyl_bucket = ifelse(lyl_bucket == "0 - < 0.1 years", 1, NA), 
         order_lyl_bucket = ifelse(lyl_bucket == "0.1 - 0.5", 2, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 0.5 - 1", 3, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 1 - 2", 4, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 2 - 3", 5, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 3 - 4", 6, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 4 - 5", 7, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 5 - < 6", 8, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == ">= 6 years", 9, order_lyl_bucket))

india_fs_fig2_data$lyl_bucket <- as.factor(india_fs_fig2_data$lyl_bucket)

# india fs fig2 plot
india_fs_fig2_plt <- india_fs_fig2_data %>%
  ggplot() +
   geom_col(mapping = aes(x = forcats::fct_reorder(name_1, llpp_who_2021), y = llpp_who_2021, fill = lyl_bucket), width = 0.5) +
  labs(x = "State", y = "Average Life Expectancy Gains (Years)",
       title = "State wise Average Life Expectancy Gains",
       subtitle = expression("(Reducing" ~ PM[2.5] ~ "pollution from 2021 levels to the WHO guideline in top 10 most populated states of India)"), 
       caption = "* Top 10 most populated states are ordered from most populated (Uttar Pradesh) to least populated (Tamil Nadu).") +
  scale_y_continuous(breaks = seq(0, 9, 1), limits = c(0, 9)) +
    scale_fill_manual(values = c("0 - < 0.1 years" = "#FFFFFF", 
                               "0.1 - 0.5" = "#FFE6B3", 
                               "> 0.5 - 1" = "#FFD25D", 
                               "> 1 - 2" = "#FFBA00", 
                               "> 2 - 3" = "#FF9600", 
                               "> 3 - 4" = "#FF6908", 
                               "> 4 - 5" = "#E63D23", 
                               "> 5 - < 6" = "#BD251C", 
                               ">= 6 years" = "#8C130E")) +
  coord_flip() +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust =  0.5, size = 10, face = "italic", margin = margin(b = 0.8, unit = "cm")), 
        plot.caption = element_text(size = 8, hjust = 0, face = "italic"),
        axis.line = element_line(), 
        axis.text = element_text(size = 11), 
        axis.title = element_text(size = 13), 
        axis.title.y = element_text(margin = margin(r = 0.7, unit = "cm")), 
        axis.title.x = element_text(margin = margin(t = 0.6, b = 0.6, unit = "cm")), 
        axis.ticks = element_blank()) 




