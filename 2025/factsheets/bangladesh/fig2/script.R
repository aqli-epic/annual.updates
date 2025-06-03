# read in the helper file
source("~/R/july.2025.helper.script.R")


#Figure 2: Top 10 threats to life expectancy in Bangladesh---------------------------------------

# top 10 most deadly diseases
bangladesh_fs_fig2_dataset <- gbd_results_master_2025 %>%
  filter(cause_of_death%in% c("Ambient ozone pollution","Child and maternal malnutrition","Childhood sexual abuse and bullying",
                              "Dietary risks","Drug use","HIV/AIDS and sexually transmitted infections","High alcohol use","Intimate partner violence","Low physical activity",
                              "Neglected tropical diseases and malaria","Nutritional deficiencies","Occupational risks","PM2.5 relative to WHO guideline","Self-harm and interpersonal violence","Substance use disorders", 
                              "Tobacco","Transport injuries","Non-optimal temperature","Other environmental risks","Unintentional injuries","Unsafe sex","Unsafe water, sanitation, and handwashing"),country == "Bangladesh") %>%
  slice_max(lyl, n = 10)

colnames(bangladesh_fs_fig2_dataset)[3] <- c("llpp_who_2023")

# bangladesh figure 3 data
bangladesh_fs_fig2_dataset <- bangladesh_fs_fig2_dataset %>%
   add_aqli_color_scale_buckets("lyl", "llpp_who_2023")

# bangladesh figure 2

#' figure 2 caption removed: Source: Global Burden of Disease
#' (https://vizhub.healthdata.org/gbd-results/) causes and risks data and WHO
#' Life Tables (https://apps.who.int/gho/data/node.main.LIFECOUNTRY?lang=en)
#' were used combined with the Life table method to arrive at these results.
#' 'PM2.5 relative to WHO Guideline' bar displays life years lost relative to
#' the WHO guideline as calculated by latest AQLI (2022) data, which will be
#' published in September 2024.

bangladesh_fs_fig2 <- bangladesh_fs_fig2_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(cause_of_death, llpp_who_2023),
                         y = llpp_who_2023,
                         fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)),
           width = 0.4, color = "black") +
  labs(x = "Threats to Life Expectancy", y = "Life Years Lost", fill = "Life years lost",
       title = "") +
  coord_flip() +
  ggthemes::theme_tufte() +
  themes_aqli_base +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 20, color="#222222"),
        axis.title.y = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"), color="#222222"),
        axis.title.x = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color="#222222"),
        plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 0.8, unit = "cm")),
        plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 0.8, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 0.8, unit = "cm")),
        legend.box.background = element_rect(color = "black"),
        plot.background = element_rect(fill = "white", color = "white"),axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(size = 24, color="#222222"),
        legend.title = element_text(size = 24, color="#222222"),
        panel.grid.major.y = element_blank()) +
  scale_y_continuous(breaks = seq(0, 7, 1)) +
  scale_fill_manual(values=c(
    "0 to < 0.1" = "#FFFFFF", 
    "0.1 to < 0.5" = "#FFF2E1", 
    "0.5 to < 1" = "#FFEDD3", 
    "1 to < 2" = "#FFC97A", 
    "2 to < 3" = "#FFA521", 
    "3 to < 4" = "#EB6C2A", 
    "4 to < 5" = "#D63333", 
    "5 to < 6" = "#8E2946", 
    ">= 6" = "#451F59")) +
  guides(fill = guide_legend(nrow = 1))
