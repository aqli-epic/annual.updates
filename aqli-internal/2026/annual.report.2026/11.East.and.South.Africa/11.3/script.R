# read in the helper file
source("~/R/july.2026.helper.script.R")

e_s_africa_agg <- gadm2_aqli_2024 %>%
  filter(country %in% eastern_and_south_african_countries, !is.na(population)) %>%
  mutate(pop_weights = population / sum(population, na.rm = TRUE)) %>%
  mutate(across(starts_with("pm"), ~ .x * pop_weights, .names = "{col}_weighted")) %>%
  summarise(across(ends_with("weighted"), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = pm1998_weighted:pm2024_weighted,names_to = "years",values_to = "pop_weighted_avg_pm25") %>%
  mutate(years = as.integer(str_extract(years, "\\d+")),series = "East and South Africa") %>%
  select(years, series, pop_weighted_avg_pm25)

# ---------------------------------------------------------
# 2. COUNTRY-LEVEL POPULATION-WEIGHTED TIME SERIES
# ---------------------------------------------------------
e_s_africa_country_ts <- gadm2_aqli_2024 %>%
  filter(country %in% eastern_and_south_african_countries, !is.na(population)) %>%
  group_by(country) %>%
  mutate(pop_weights = population / sum(population, na.rm = TRUE)) %>%
  mutate(across(starts_with("pm"), ~ .x * pop_weights, .names = "{col}_weighted")) %>%
  summarise(across(ends_with("weighted"), ~ sum(.x, na.rm = TRUE)),.groups = "drop") %>%
  pivot_longer(cols = pm1998_weighted:pm2024_weighted,names_to = "years",values_to = "pop_weighted_avg_pm25") %>%
  mutate(years = as.integer(str_extract(years, "\\d+")),series = country) %>%
  select(years, series, pop_weighted_avg_pm25)

# ---------------------------------------------------------
# 3. COMBINE + KEEP ONLY REQUIRED SERIES
# ---------------------------------------------------------
e_s_africa_fs_fig_dataset <- bind_rows(e_s_africa_agg,e_s_africa_country_ts) %>%
  filter(series %in% c("East and South Africa","Uganda","Zambia","Lesotho","Tanzania","South Africa"))

# set legend order
e_s_africa_fs_fig_dataset$series <- factor(e_s_africa_fs_fig_dataset$series,
  levels = c("East and South Africa","Uganda","Zambia","Lesotho","Tanzania","South Africa")
)

# ---------------------------------------------------------
# 4. PLOT
# ---------------------------------------------------------
e_s_africa_fs_fig <- ggplot(e_s_africa_fs_fig_dataset,
  aes(x = years,y = pop_weighted_avg_pm25,color = series,linetype = series,size = series )) +
  geom_line() +
  
  # WHO guideline line (not included in legend)
  geom_hline( yintercept = 5, linewidth = 0.8, linetype = "dotted", color = "black", show.legend = FALSE) +
  scale_color_manual(values = c("East and South Africa"= "#1e3a8a",
                                "Uganda"= "#0f172a", "Zambia"= "#94a3b8","Lesotho"= "#9ca3af", "Tanzania"="#2563eb",
                                "South Africa"="#0ea5a4")) +
  scale_linetype_manual(values = c(
    "East and South Africa" = "solid",
    "Uganda"     = "dashed",
    "Zambia"  = "dotted",
    "Lesotho"  = "dotdash",
    "Tanzania"  = "longdash",
    "South Africa"= "solid"
  )) +
  scale_size_manual(values = c(
    "East and South Africa" = 1.4,
    "Uganda"     = 1.1,
    "Zambia"  = 1.1,
    "Lesotho"   = 0.9,
    "Tanzania"   = 0.9,
    "South Africa"= 0.9
  )) +
  scale_y_continuous(breaks = seq(0, 45, 5),limits = c(0, 45) ) +
  scale_x_continuous(breaks = seq(1998, 2024, 2) ) +
  annotate( "text",x = 2003.6,y = 6,label = expression("WHO " * PM[2.5] * " Guideline (last updated: 2021): 5 µg/m³"),size = 15 / .pt)+
  ggthemes::theme_tufte() +
  labs(x = "Year", y = expression("Annual Average " * PM[2.5] * " Concentration (in " * mu * "g/m"^3 * ")") ) +
  guides(size = "none") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16, color = "#222222"),
    legend.title = element_blank(),
    axis.text = element_text(size = 20, color = "#222222"),
    axis.title.y = element_text(size = 24,margin = margin(r = 0.6, unit = "cm"),  color = "#222222"),
    axis.title.x = element_text(size = 24,margin = margin(t = 0.6, b = 0.6, unit = "cm"),color = "#222222"),
    axis.line = element_line(),
    legend.box.background = element_rect(color = "black"),
    plot.title = element_text(hjust = 0.5,size = 16,margin = margin(b = 0.7, unit = "cm")),
    plot.subtitle = element_text(hjust = 0.5,size = 8,face = "italic"),
    plot.caption = element_text(size = 7,margin = margin(t = 0.8, unit = "cm"),hjust = 0,face = "italic"),
    plot.background = element_rect(color = "white"),axis.ticks = element_blank()
    )