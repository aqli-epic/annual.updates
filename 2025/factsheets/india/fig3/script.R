# read in the helper file
source("R/july_2025_helper_script.R")

# Fig 3:  GBD India graph --------------------------------------------

# top external health threats
disease_list <- c("PM2.5 relative to WHO guideline",
                  "Dietary risks",
                  "Child and maternal malnutrition",
                  "Tobacco",
                  "Unsafe water, sanitation, and handwashing",
                  "Unintentional injuries",
                  "Occupational risks",
                  "Non-optimal temperature",
                  "Transport injuries",
                  "Self-harm and interpersonal violence")

india_fs_fig3_dataset <- gbd_results_master_2023 %>%
  filter(country == "India",
         cause_of_death %in% disease_list)

colnames(india_fs_fig3_dataset)[3] <- c("llpp_who_2023")

india_fs_fig3_dataset <- india_fs_fig3_dataset %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2023")

# figure 3
# Source: Global Burden of Disease (https://vizhub.healthdata.org/gbd-results/)
# causes and risks data and WHO Life Tables (https://apps.who.int/gho/data/node.main.LIFECOUNTRY?lang=en)
# were used combined with the Life table method to arrive at these results.
# 'PM2.5 relative to WHO Guideline' bar displays life years lost relative to the
# WHO guideline as calculated by latest AQLI (2023) data, which will be published
# in 2024.

india_fs_fig3 <- india_fs_fig3_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(cause_of_death, llpp_who_2023),
                         y = llpp_who_2023,
                         fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)),
           width = 0.5) +
  labs(x = "Threats to Life Expectancy", y = "Life Years Lost", fill = "Life years lost",
       title = expression("")) +
  coord_flip() +
  ggthemes::theme_tufte() +
  themes_aqli_base +
  theme(axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16, margin = margin(r = 0.6, unit = "cm")),
        axis.title.x = element_text(size = 16, margin = margin(t = 0.6, b = 0.6, unit = "cm")),
        axis.line = element_line(),
        plot.background = element_rect(fill = "white", color = "white")) +
  scale_y_continuous(breaks = seq(0, 6, 0.5)) +
  scale_fill_manual(values = c("0 to < 0.1" = "#FFFFFF",
                               "0.1 to < 0.5" = "#FFF2E1",
                               "0.5 to < 1" = "#FFEDD3",
                               "1 to < 2" = "#FFC97A",
                               "2 to < 3" = "#FFA521",
                               "3 to < 4" = "#EB6C2A",
                               "4 to < 5" = "#D63333",
                               "5 to < 6" = "#8E2946",
                               ">= 6" = "#451F59")) +
  guides(fill = guide_legend(nrow = 1))
