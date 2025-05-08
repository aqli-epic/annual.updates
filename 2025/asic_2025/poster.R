#### set up ####
# libraries
library(readxl)
library(tidyverse)
library(ggthemes)

# global variables
who_guideline <- 5
le_constant <- 0.098
latest_year <- 2022
first_year <- 1998

# global operations
`%notin%` <- Negate(`%in%`)

# colour scale
source("~/Desktop/AQLI/REPO/annual.updates/R/colour_scale.R")

#### data #### 
# read in latest PM2.5 data file
gadm0_aqli_2022 <- read_csv("~/Desktop/AQLI/2024 AQLI Update/data/gadm0_2022_wide.csv")
public_data <- read_csv("~/Desktop/AQLI/2024 AQLI Update/website/open_data.csv")

nat_standard <- read_excel("~/Desktop/AQLI/nature communications/aq_standards.xlsx") %>%
  select(Country, `National Annual Average PM2.5 Standard (in µg/m³)`, `Year first adopted`) %>%
  rename(natstandard = `National Annual Average PM2.5 Standard (in µg/m³)`,
         year_adopt = `Year first adopted`)

opp_score <- read_csv("~/Desktop/AQLI/REPO/opportunity-score/data/output/opportunity_score.csv")

plot_data <- gadm0_aqli_2022 %>%
  select(name, population, pm2022, who2022) %>%
  left_join(nat_standard, by = c("name" = "Country")) %>%
  inner_join(opp_score %>% select(country, monitor_density), by = c("name" = "country"))

plot_data <- plot_data %>%
  mutate(nat_std_qtile = case_when(natstandard <= 15 ~ 1,
                                   natstandard > 15 & natstandard <= 30 ~ 2, 
                                   natstandard > 30 ~ 3,
                                   is.na(natstandard) ~ 4),
         mon_dens_qtile = case_when(is.na(monitor_density) ~ 1,
                                    monitor_density < 0.1 ~ 2,
                                    monitor_density >= 0.1 & monitor_density < 1 ~ 3, 
                                    monitor_density >= 1 & monitor_density < 4 ~ 4, 
                                    monitor_density >= 4 & monitor_density < 10 ~ 5,
                                    monitor_density >= 10 ~ 6)) %>%
  replace_na(list(nat_std_qtile = 6, mon_dens_qtile = 0)) %>%
  add_aqli_color_scale_buckets("lyl", "who2022")

plot_data %>%
  ggplot(aes(x = mon_dens_qtile, y = nat_std_qtile, size = lyl_bucket, fill = lyl_bucket)) +
  geom_jitter(width = 0.5, height = 0.5, alpha = 0.7, shape = 21) +
  geom_hline(yintercept = 1, linetype = "solid", lwd = 2) +
  geom_vline(xintercept = 3.1, linetype = "solid", lwd = 2) +
  # annotate("text", x = -1, y = 5.5, label = "Relaxed Standard\n+ Low Monitoring", hjust = 0) +
  # annotate("text", x = 7, y = 5.5, label = "Relaxed Standard\n+ High Monitoring", hjust = 1) +
  # annotate("text", x = -1, y = 2.5, label = "Strict Standard\n+ Low Monitoring", hjust = 0) +
  # annotate("text", x = 7, y = 2.5, label = "Strict Standard\n+ High Monitoring", hjust = 1) +
  scale_fill_manual(values = c("0 to < 0.1" = "#FFFFFF", 
                               "0.1 to < 0.5" = "#FFF2E1", 
                               "0.5 to < 1" = "#FFEDD3", 
                               "1 to < 2" = "#FFC97A", 
                               "2 to < 3" = "#FFA521", 
                               "3 to < 4" = "#EB6C2A", 
                               "4 to < 5" = "#D63333", 
                               "5 to < 6" = "#8E2946", 
                               ">= 6" = "#451F59")) +
  scale_size_manual(values = c("0 to < 0.1" = 2,
                               "0.1 to < 0.5" = 4,
                               "0.5 to < 1" = 6,
                               "1 to < 2" = 8,
                               "2 to < 3" = 10,
                               "3 to < 4" = 12,
                               "4 to < 5" = 14,
                               "5 to < 6" = 16,
                               ">= 6" = 18)) +
  guides(size = guide_legend(title = "Potential gain in \nlife expectancy (years)"),
         fill = guide_legend(title = "Potential gain in \nlife expectancy (years)")) +
  labs(x = "Monitor Density (Less Dense to More Dense)", y = "National Standard (Strict to Relaxed)") +
  theme_tufte() +
  theme(legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 20, margin = margin(r = 0.3, unit = "cm")),
        axis.title.y = element_text(size = 20, margin = margin(r = 0.3, unit = "cm")))

