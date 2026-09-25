#### set up ####
# libraries
library(readxl)
library(tidyverse)
library(ggthemes)
library(patchwork)

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
  mutate(nat_std_qtile = case_when(is.na(natstandard) ~ 1,
                                   natstandard > 30 ~ 2,
                                   natstandard <= 30 & natstandard > 15 ~ 3,
                                   natstandard <= 15 ~ 4),
         mon_dens_qtile = case_when(is.na(monitor_density) ~ 1,
                                    monitor_density < 0.1 ~ 2,
                                    monitor_density >= 0.1 & monitor_density < 1 ~ 3,
                                    monitor_density >= 1 & monitor_density < 4 ~ 4,
                                    monitor_density >= 4 & monitor_density < 10 ~ 5,
                                    monitor_density >= 10 ~ 6)) %>%
  # replace_na(list(natstandard = 60, monitor_density = 0)) %>%
  add_aqli_color_scale_buckets("lyl", "who2022")

plot_data %>%
  summarise(tot_lyl = sum(who2022*population, na.rm = TRUE))

plot_data %>%
  mutate(quad = if_else(nat_std_qtile > 3 & mon_dens_qtile >= 4, "std <= 15, monitors >= 1 mil", NA),
         quad = if_else(nat_std_qtile > 3 & mon_dens_qtile < 4, "std <= 15, monitors < 1 mil", quad),
         quad = if_else(nat_std_qtile <= 3 & mon_dens_qtile < 4, "std > 15, monitors < 1 mil", quad),
         quad = if_else(nat_std_qtile <= 3 & mon_dens_qtile >= 4, "std > 15, monitors >= 1 mil", quad)) %>%
  group_by(quad) %>%
  summarise(tot_lyl = sum(who2022*population, na.rm = TRUE)) %>%
  mutate(percent = 100*tot_lyl/14532250594)

plot_data %>%
  ggplot(aes(x = mon_dens_qtile, y = nat_std_qtile, fill = lyl_bucket)) +
  geom_hline(yintercept = 3, linetype = "solid", lwd = 2, colour = "grey") +
  geom_vline(xintercept = 4, linetype = "solid", lwd = 2, colour = "grey") +
  # geom_point(alpha = 0.7, shape = 21, size = 10) +
  geom_jitter(width = 0.5, height = 0.5, shape = 21, size = 10, colour = "grey") +
  # geom_text(aes(label = name), size = 6, fontface = "bold", check_overlap = TRUE) +
  annotate("text", x = -1, y = 5.5, hjust = 0, size = 6,
           label = "National Standard <= 15, \nMonitor Density < 1") +
  annotate("text", x = 8.5, y = 5.5, hjust = 1, size = 6,
           label = "National Standard <= 15, \nMonitor Density >= 1") +
  annotate("text", x = -1, y = 0.5, hjust = 0, size = 6,
           label = "National Standard > 15, \nMonitor Density < 1") +
  annotate("text", x = 8.5, y = 0.5, hjust = 1, size = 6,
           label = "National Standard > 15, \nMonitor Density >= 1")+
  annotate("text", x = -1, y = 5.25, hjust = 0, size = 7.5, fontface = "bold",
           label = "PGLE Share: 4.6%") +
  annotate("text", x = 8.5, y = 5.25, hjust = 1, size = 7.5, fontface = "bold",
           label = "PGLE Share: 5.7%") +
  annotate("text", x = -1, y = 0.75, hjust = 0, size = 7.5, fontface = "bold",
           label = "PGLE Share: 61.4%") +
  annotate("text", x = 8.5, y = 0.75, hjust = 1, size = 7.5, fontface = "bold",
           label = "PGLE Share: 28.2%") +
  scale_fill_manual(values = c("0 to < 0.1" = "#FFFFFF",
                               "0.1 to < 0.5" = "#FFF2E1",
                               "0.5 to < 1" = "#FFEDD3",
                               "1 to < 2" = "#FFC97A",
                               "2 to < 3" = "#FFA521",
                               "3 to < 4" = "#EB6C2A",
                               "4 to < 5" = "#D63333",
                               "5 to < 6" = "#8E2946",
                               ">= 6" = "#451F59"),
                    labels = c("0 to < 0.1  ",
                               "0.1 to < 0.5  ",
                               "0.5 to < 1  ",
                               "1 to < 2  ",
                               "2 to < 3  ",
                               "3 to < 4  ",
                               "4 to < 5  ",
                               "5 to < 6  ",
                               ">= 6  ")) +
  guides(#size = guide_legend(title = "Potential gain in \nlife expectancy \n(PGLE, Years)"),
         fill = guide_legend(title = "Potential gain in life expectancy (PGLE, Years)",
                             title.position = "top",
                             title.hjust = 0.5,  # center the title
                             nrow = 1, byrow = TRUE)) +
  labs(x = "Monitor Density (per million people)", y = "National Standard (in µg/m³)") +
  theme_tufte() +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box = "vertical",
        legend.box.background = element_rect(color = "grey"),
        legend.text = element_text(size = 32),
        legend.title = element_text(size = 32),
        # legend.spacing.x = unit(1.5, "cm"),
        # legend.key.width = unit(1, "cm"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 40, margin = margin(r = 0.3, unit = "cm")),
        axis.title.y = element_text(size = 40, margin = margin(r = 0.3, unit = "cm")))
