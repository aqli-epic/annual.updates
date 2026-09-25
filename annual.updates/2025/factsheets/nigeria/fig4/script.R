# read in the helper file
source("R/july.2025.helper.script.R")

# Figure 4: Top external threats to life expectancy in Nigeria
nigeria_fs_fig3_data <- gbd_results_master_2023 %>%
  filter(country == "Nigeria",
         cause_of_death %in% c("Child and maternal malnutrition",
                               "Neglected tropical diseases and malaria",
                               "PM2.5 relative to WHO guideline",
                               "Unsafe water, sanitation, and handwashing",
                               "HIV/AIDS and sexually transmitted infections",
                               "Dietary risks",
                               "Unsafe sex",
                               "Unintentional injuries",
                               "High alcohol use",
                               "Self-harm and interpersonal violence"))

colnames(nigeria_fs_fig3_data)[3] <- c("llpp_who_2023")

nigeria_fs_fig3_data <- nigeria_fs_fig3_data %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2023")

# figure 3
# Source: Global Burden of Disease (https://vizhub.healthdata.org/gbd-results/)
# causes and risks data and WHO Life Tables (https://apps.who.int/gho/data/node.main.LIFECOUNTRY?lang=en)
# were used combined with the Life table method to arrive at these results.
# 'PM2.5 relative to WHO Guideline' bar displays life years lost relative to the
# WHO guideline as calculated by latest AQLI (2023) data, which will be published
# in 2024.

nigeria_fs_fig3 <- nigeria_fs_fig3_data %>%
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
  scale_y_continuous(breaks = seq(0, 3.5, 0.5), limits = c(0, 3.5)) +
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

