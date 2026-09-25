################################################################################
# Ecuador Factsheet – Figure 4: Annual Average PM2.5 Trends by Region
################################################################################

# --- 1. Define Ecuador's regions by natural geography ---------

euc_Sierra_provinces <- c(
  "Azuay", "Bolivar", "Cañar", "Carchi", "Chimborazo", "Cotopaxi", 
  "Imbabura", "Loja", "Pichincha", "Tungurahua"
)

euc_Costa_provinces <- c(
  "El Oro", "Esmeraldas", "Guayas", "Los Rios", 
  "Manabi", "Santa Elena", "Santo Domingo de los Tsachilas"
)

euc_Amazonia_provinces <- c(
  "Morona Santiago", "Napo", "Orellana", 
  "Pastaza", "Sucumbios", "Zamora Chinchipe"
)

euc_Insular_provinces <- c("Galápagos")  # Insular region


# --- 2. Filter Ecuador data and assign regions based on province names -------

ecuador_aqli_2023 <- gadm2_aqli_2023 %>%
  filter(country == "Ecuador") %>%
  mutate(region = case_when(
    name_1 %in% euc_Sierra_provinces   ~ "Sierra",
    name_1 %in% euc_Costa_provinces    ~ "Costa",
    name_1 %in% euc_Amazonia_provinces ~ "Amazonía",
    name_1 %in% euc_Insular_provinces  ~ "Insular"
  ))


# --- 3. Figure 4 Data Preparation: Regional Trends ---------------------------

# Calculate population-weighted PM2.5 averages by region for all years
ecuador_fs_fig4_regions <- ecuador_aqli_2023 %>%
  group_by(region) %>%
  mutate(
    pop_weights = population / sum(population, na.rm = TRUE),
    across(starts_with("pm"), ~ .x * pop_weights, .names = "{col}_weighted")
  ) %>%
  summarise(across(ends_with("weighted"), sum), .groups = "drop") %>%
  pivot_longer(
    cols = pm1998_weighted:pm2023_weighted,
    names_to = "years",
    values_to = "pop_weighted_avg_pm2.5"
  ) %>%
  mutate(
    years = as.integer(unlist(str_extract(years, "\\d+")))
  ) %>%
  select(years, region, pop_weighted_avg_pm2.5) %>%
  arrange(years)


# --- 4. Figure 4 Data Preparation: National Average --------------------------

ecuador_fs_fig4_nat <- ecuador_aqli_2023 %>%
  mutate(
    pop_weights = population / sum(population, na.rm = TRUE),
    across(starts_with("pm"), ~ .x * pop_weights, .names = "{col}_weighted")
  ) %>%
  summarise(across(ends_with("weighted"), sum), .groups = "drop") %>%
  pivot_longer(
    cols = pm1998_weighted:pm2023_weighted,
    names_to = "years",
    values_to = "pop_weighted_avg_pm2.5"
  ) %>%
  mutate(
    years = as.integer(unlist(str_extract(years, "\\d+"))),
    region = "National Average"
  ) %>%
  select(years, region, pop_weighted_avg_pm2.5)


# --- 5. Combine Regional and National Trends ---------------------------------

ecuador_fs_fig4_dataset <- bind_rows(ecuador_fs_fig4_regions, ecuador_fs_fig4_nat)

ecuador_fs_fig4_dataset$region <- factor(
  ecuador_fs_fig4_dataset$region,
  levels = c("National Average", "Amazonía", "Costa", "Sierra", "Insular")
)


# --- 6. Plot: Ecuador Factsheet Figure 4 -------------------------------------

ecuador_fs_fig4 <- ecuador_fs_fig4_dataset %>%
  ggplot() +
  geom_line(
    aes(x = years,
        y = pop_weighted_avg_pm2.5,
        colour = region,
        linetype = region),
    lwd = 1.1
  ) +
  
  # WHO (2021) and Ecuador PM2.5 thresholds
  geom_hline(aes(yintercept = 5),  lwd = 0.8, linetype = "dotted") +  # WHO guideline
  geom_hline(aes(yintercept = 15), lwd = 0.8, linetype = "dotted") +  # Ecuador standard
  
  # Axes
  scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 35)) +
  scale_x_continuous(breaks = c(seq(1998, 2020, 2), 2023)) +
  
  # Colors and linetypes
  scale_color_manual(values = c(
    "National Average" = "#145566",
    "Amazonía"         = "#217D91",
    "Costa"            = "#3DB1C8",
    "Sierra"           = "#79CAD9",
    "Insular"          = "#A6DEEA"
  )) +
  scale_linetype_manual(values = c(
    "National Average" = "solid",
    "Amazonía"         = "dashed",
    "Costa"            = "dashed",
    "Sierra"           = "dashed",
    "Insular"          = "dashed"
  )) +
  
  # Styling and theme
  ggthemes::theme_tufte() +
  labs(
    x = "Year",
    y = expression("Annual Average" ~ PM[2.5] ~ "Concentration (in µg/m³)")
  ) +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 24, color = "#222222"),
    legend.title    = element_blank(),
    axis.title.y    = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"), color = "#222222"),
    axis.title.x    = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color = "#222222"),
    axis.text       = element_text(size = 20, color = "#222222"),
    axis.ticks      = element_blank(),
    axis.line       = element_line(),
    plot.background = element_rect(color = "white"),
    legend.box.background = element_rect(color = "black"),
    plot.title      = element_text(hjust = 0.5, size = 16, margin = margin(b = 0.7, unit = "cm")),
    plot.subtitle   = element_text(hjust = 0.5, size = 8, face = "italic"),
    plot.caption    = element_text(size = 7, margin = margin(t = 0.8, unit = "cm"), hjust = 0, face = "italic")
  ) +
  
  # Add text annotations for thresholds
  geom_text(x = 2016, y = 4,
            label = expression("WHO" ~ PM[2.5] ~ "Guideline (last updated: 2021): 5 µg/m³"),
            size = 7) +
  geom_text(x = 2017, y = 14,
            label = expression("Ecuador National" ~ PM[2.5] ~ "Standard: 15 µg/m³"),
            size = 7)

