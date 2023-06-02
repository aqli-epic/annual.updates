
# read in the helper file (look for appPublic folder in the root of the repository)
source("./appPublic/aqli.data.explorer.helper.script.R")

# filtering results for India
india_fs_fig3_dataset <- gbd_results_master_2021 %>%
  filter(country == "India")

colnames(india_fs_fig3_dataset)[3] <- c("llpp_who_2021")

# india fs fig3 dataset
india_fs_fig3_dataset <- india_fs_fig3_dataset %>%
    mutate(lyl_bucket = ifelse((llpp_who_2021 >= 0) & (llpp_who_2021 < 0.1), "0 - < 0.1 years", NA), 
         lyl_bucket = ifelse((llpp_who_2021 >= 0.1) & (llpp_who_2021 <= 0.5), "0.1 - 0.5", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 0.5) & (llpp_who_2021 <= 1), "> 0.5 - 1", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 1) & (llpp_who_2021 <= 2), "> 1 - 2", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 2) & (llpp_who_2021 <= 3), "> 2 - 3", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 3) & (llpp_who_2021 <= 4), "> 3 - 4", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 4) & (llpp_who_2021 <= 5), "> 4 - 5", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 > 5) & (llpp_who_2021 < 6), "> 5 - < 6 years", lyl_bucket), 
         lyl_bucket = ifelse((llpp_who_2021 >= 6), ">= 6 years", lyl_bucket)) %>%
  mutate(order_lyl_bucket = ifelse(lyl_bucket == "0 - < 0.1 years", 1, NA), 
         order_lyl_bucket = ifelse(lyl_bucket == "0.1 - 0.5", 2, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 0.5 - 1", 3, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 1 - 2", 4, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 2 - 3", 5, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 3 - 4", 6, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 4 - 5", 7, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == "> 5 - 6 years", 8, order_lyl_bucket), 
         order_lyl_bucket = ifelse(lyl_bucket == ">= 6 years", 9, order_lyl_bucket))


# getting disease (ordered from most deadly to least deadly) in a vector (to be used later in the
# scale_x_discrete function).

# cause_of_death_ordered <- india_fs_fig3_dataset %>%
#   arrange(llpp_who_2021) %>%
#   pull(cause_of_death) %>%
#   unlist()


# figure 3 plot
india_fs_fig3 <- india_fs_fig3_dataset %>%
  ggplot() + 
  geom_col(mapping = aes(x = forcats::fct_reorder(cause_of_death, llpp_who_2021), y = llpp_who_2021, fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), width = 0.5, color = "black") +
  labs(x = "Cause of Death", y = "Life Years Lost", fill = "Life years lost", 
       caption = str_wrap("* Source: Global Burden of Disease (https://vizhub.healthdata.org/gbd-results/) causes and risks data and WHO Life Tables (https://apps.who.int/gho/data/node.main.LIFECOUNTRY?lang=en) were used combined with the Life table method to arrive at these results. 'PM2.5 relative to WHO Guideline' bar displays life years lost relative to the WHO guideline as calculated by latest AQLI (2021) data, which will be published in September 2023.", width = 130), title = expression("Life Expectancy Impact of" ~ PM[2.5] ~ "and Unassociated Causes/Risks of Deaths in India")) +
  coord_flip() + 
  ggthemes::theme_tufte() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16, margin = margin(r = 0.6, unit = "cm")),
        axis.title.x = element_text(size = 16, margin = margin(t = 0.6, b = 0.6, unit = "cm")),
        plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 0.8, unit = "cm")),
        plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 0.8, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 0.8, unit = "cm")),
        legend.box.background = element_rect(color = "black"),
        plot.background = element_rect(color = "white"),
        axis.line = element_line(), 
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 14),
        panel.grid.major.y = element_blank()) +
  scale_y_continuous(breaks = seq(0, 6, 0.5)) +
 # scale_x_discrete(limits = cause_of_death_ordered[seq(1, length(cause_of_death_ordered), by = 2)]) +
  scale_fill_manual(values = c("0 - < 0.1 years" = "#FFFFFF", 
                               "0.1 - 0.5" = "#FFE6B3", 
                               "> 0.5 - 1" = "#FFD25D", 
                               "> 1 - 2" = "#FFBA00", 
                               "> 2 - 3" = "#FF9600", 
                               "> 3 - 4" = "#FF6908", 
                               "> 4 - 5" = "#E63D23", 
                               "> 5 - < 6 years" = "#BD251C", 
                               ">= 6 years" = "#8C130E")) + 
  guides(fill = guide_legend(nrow = 1))


