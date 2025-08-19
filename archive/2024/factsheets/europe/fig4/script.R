# read in the helper file
source("R/july.2024.helper.script.R")

#Figure 4:  Annual average PM2.5 concentration in Europe, 1998-2022

# create a europe AQLI dataset
gadm2_aqli_2022_europe <- gadm2_aqli_2022 %>%
  filter(country %in% european_countries$country)%>%
  mutate(region = case_when(
    country %in% eu_countries ~ "European Union",
    country %notin% eu_countries ~ "Non European Union"))

# europe pop
europe_pop <- sum(gadm2_aqli_2022_europe$population, na.rm =  TRUE)

# pop living above who guideline
europe_above_who <- gadm2_aqli_2022_europe %>%
  filter(pm2022 > 10) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE))


# country wise average and how many are above WHO
europe_country_wise <- gadm2_aqli_2022_europe %>%
  gadm_level_summary(c("country"), c(2022), 10)


# exclude the following countries to keep the map less wide and to show a stark difference between eastern and western europe
exclude_countries <-  c("Russia", "Turkey", "Sweden", "Finland", "Norway", "Kazakhstan", "Iceland", "Georgia", "Azerbaijan", "Armenia", "Cyprus", "Northern Cyprus", "Svalbard and Jan Mayen")


countries_except_excluded <- european_countries %>%
  filter(country %notin% exclude_countries)

# creating region wise average PM2.5 data from 1998 to 2022
europe_fs_fig4_regions <- gadm2_aqli_2022_europe %>%
  group_by(region) %>%
  filter(!is.na(population)) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2022_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  select(years,region, pop_weighted_avg_pm2.5)

europe_fs_fig4_data <- gadm2_aqli_2022_europe %>%
  filter(!is.na(population)) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2022_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "Europe") %>%
  select(years,region, pop_weighted_avg_pm2.5)

europe_fs_fig4_dataset <- rbind(europe_fs_fig4_regions, europe_fs_fig4_data)

europe_fs_fig4_dataset$region = factor(europe_fs_fig4_dataset$region, levels = c('Europe', 'European Union', 'Non European Union'))

# fig 4
europe_fs_fig4 <- europe_fs_fig4_dataset %>%
  ggplot() +
  geom_line(mapping = aes(x = as.integer(years),
                          y = as.double(pop_weighted_avg_pm2.5),
                          colour = region, linetype = region), lwd = 1.1) +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dotted") +
  geom_hline(mapping = aes(yintercept = 10), lwd = 0.8, linetype = "dotted") +
  geom_hline(mapping = aes(yintercept = 25), lwd = 0.8, linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30)) +
  scale_x_continuous(breaks = c(seq(1998, 2019, 3), 2022)) +
  scale_color_manual(values = c("Europe" = "#7197be", "European Union" = "#5f7aa5", "Non European Union" = "#CBE8F3")) +
  scale_linetype_manual(values = c("Europe" = "solid", "European Union" = "dashed", "Non European Union" = "dashed")) +
  ggthemes::theme_tufte() +
  labs(x = "Year",
       y = expression("Annual Average   " ~ PM[2.5] ~ " Concentration (in µg/m³)"),
       title = "") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 20, color="#222222"),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"), color="#222222"),
        axis.title.x = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color="#222222"),
        axis.line = element_line(),
        legend.box.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 0.7, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 8, face = "italic"),
        plot.caption = element_text(size = 7, margin = margin(t = 0.8, unit = "cm"), hjust = 0, face = "italic"),
        axis.text = element_text(size = 20, color="#222222"),
        plot.background = element_rect(color = "white", fill = "white"),
        axis.ticks = element_blank()) +
  geom_text(x = 2001.5, y = 5.7, label = expression("WHO" ~ PM[2.5] ~ "Guideline (last updated: 2021): 5 µg/m³"), size = 4.5)+
  geom_text(x = 2001.2, y = 25.7, label = expression("European Union " ~ PM[2.5] ~ "Standard: 25 µg/m³"), size = 5)+
  geom_text(x = 2001.2, y = 10.7, label = expression("European Union " ~ PM[2.5] ~ "2030 targets: 10 µg/m³"), size = 5)
