# ------------------------------------------------------------------------------
# AQLI 2026 – Global PM2.5 Trend Analysis
# Description : Computes population-weighted global PM2.5 and generates a 
#               time series plot with key trend periods.
#
# Developed By: Purushottam Gupta
# Role        : Data Analyst/Data Architect, AQLI (University of Chicago)
# Contact     : guptap@uchicago.edu
# Dependencies: dplyr, tidyr, ggplot2, stringr

# ------------------------------------------------------------------------------
# Load Dependencies & Source Files
# ------------------------------------------------------------------------------
source("~R/helper_function_2026.R")

# ------------------------------------------------------------------------------
# Data Preparation: Global Aggregation (Population-weighted PM2.5)
# ------------------------------------------------------------------------------
gadm0_aqli_2024 <- gadm0_aqli_2024 %>%
  mutate(global = "Global")

global_pm <- gadm0_aqli_2024 %>%
  group_by(global) %>%
  summarise(
    across(
      starts_with("pm"),
      ~ round(
        sum(.x * population, na.rm = TRUE) /
          sum(population, na.rm = TRUE),
        2
      )
    ),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(global, starts_with("pm"))

# ------------------------------------------------------------------------------
# Data Transformation: Long Format for Time Series
# ------------------------------------------------------------------------------
df_global <- global_pm %>%
  pivot_longer(
    cols = -global,
    names_to = "year",
    values_to = "pm"
  ) %>%
  mutate(
    year = str_extract(year, "\\d+") %>% as.integer()
  )

# ------------------------------------------------------------------------------
# Plot Parameters
# ------------------------------------------------------------------------------
y_max <- ceiling(max(df_global$pm, na.rm = TRUE))

# ------------------------------------------------------------------------------
# Visualization: Global PM2.5 Trend
# ------------------------------------------------------------------------------
final_plot <- ggplot(df_global, aes(x = year, y = pm)) +
  
  # --- Background Shading: Time Periods ---
  annotate("rect", xmin = -Inf, xmax = 2015,
           ymin = -Inf, ymax = Inf,
           fill = "white", alpha = 0.6) +
  
  annotate("rect", xmin = 2015, xmax = 2020,
           ymin = -Inf, ymax = Inf,
           fill = "#F4F6F6", alpha = 0.6) +
  
  annotate("rect", xmin = 2020, xmax = 2024.5,
           ymin = -Inf, ymax = Inf,
           fill = "#F7FAFF", alpha = 0.7) +
  
  # --- Period Annotations ---
  annotate("text", x = 2006, y = y_max,
           label = "Period of consistent increase in\nglobal particulate levels",
           family = "Arial", size = 4) +
  
  annotate("text", x = 2017.5, y = y_max + 1,
           label = "Period of global \nclean air progress",
           family = "Arial", size = 4) +
  
  annotate("text", x = 2022.5, y = y_max - 3,
           label = "Period of stagnation in\nglobal particulate levels",
           family = "Arial", size = 4) +
  
  # --- Trend Line ---
  geom_line(
    color = "#1F4E8C",
    linewidth = 1
  ) +
  
  # --- Structural Break Lines ---
  geom_vline(xintercept = c(2015, 2020),
             linetype = "dotted",
             linewidth = 0.8,
             color = "#454545") +
  
  # --- Scales ---
  scale_x_continuous(
    breaks = seq(1998, 2024, by = 2)
  ) +
  
  scale_y_continuous(
    limits = c(10, y_max + 10),
    breaks = seq(10, y_max + 10, by = 10),
    expand = c(0, 0)
  ) +
  
  # --- Labels ---
  labs(
    x = "Year",
    y = expression(
      "Annual Average PM"[2.5] *
        " Concentration (" * mu * "g/m"^3 * ")"
    )
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    
    axis.title = element_text(size = 18, color = "#222222"),
    axis.title.y = element_text(margin = ggplot2::margin(r = 18)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 18)),
    
    plot.margin = margin(20, 80, 20, 20),
    
    axis.text = element_text(size = 18, color = "#222222"),
    axis.line = element_line(color = "#222222"),
    axis.ticks = element_blank(),
    
    # 🔴 REMOVE EVERYTHING FROM PANEL
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    
    plot.background = element_rect(fill = "white", color = "white")
  )

