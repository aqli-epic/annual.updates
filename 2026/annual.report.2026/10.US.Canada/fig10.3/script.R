# ------------------------------------------------------------------------------
# AQLI 2026 – US & Canada PM2.5 Trend Comparison
# Description : Visualizes annual average PM2.5 trends (1998–2024) for the 
#               United States and Canada with end-point labeling.
#
# Developed By: Purushottam Gupta
# Role        : Data Architect & Data Analyst, AQLI (University of Chicago)
# Contact     : guptap@uchicago.edu
# Dependencies: dplyr, tidyr, ggplot2, ggthemes, stringr
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Load Libraries
# ------------------------------------------------------------------------------
library(arrow)
library(data.table)
library(tidyverse)

# ------------------------------------------------------------------------------
# Data Preparation
# ------------------------------------------------------------------------------

us_canada_fig10.3 <- gadm0_aqli_2024 %>%
  filter(country %in% c("United States", "Canada"))

us_canada_fig10.3$country <- factor(
  us_canada_fig10.3$country,
  levels = c("United States", "Canada")
)

us_canada_fig10.3 <- us_canada_fig10.3 %>%
  select(country, starts_with("pm")) %>%
  pivot_longer(
    !c(country),
    names_to = "year",
    values_to = "pm"
  )

us_canada_fig10.3$year <- gsub("[^0-9]", "", us_canada_fig10.3$year)
us_canada_fig10.3$year <- as.integer(us_canada_fig10.3$year)

# using PM2.5 directly as plotting variable
us_canada_fig10.3$lyl <- us_canada_fig10.3$pm

# ------------------------------------------------------------------------------
# Plot Preparation
# ------------------------------------------------------------------------------

n_countries <- length(unique(us_canada_fig10.3$country))
y_max <- ceiling(max(us_canada_fig10.3$lyl, na.rm = TRUE))

# last year points for labeling
last_points <- us_canada_fig10.3 %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  arrange(desc(lyl))

# evenly spaced labels for clean right-side alignment
last_points$label_y <- seq(
  from = max(last_points$lyl),
  to   = min(last_points$lyl),
  length.out = nrow(last_points)
)

label_x <- max(us_canada_fig10.3$year) + 1.5

# fixed AQLI palette
aqli_palette <- c("#3B5B92", "#D2693C")

# ------------------------------------------------------------------------------
# Visualization
# ------------------------------------------------------------------------------
ar_us_canada_fig10.3 <- ggplot(
  us_canada_fig10.3,
  aes(x = year, y = lyl, color = country)
) +
  geom_line(linewidth = 1.2) +
  geom_segment(
    data = last_points,
    aes(
      x = year,
      y = lyl,
      xend = label_x,
      yend = label_y
    ),
    color = "lightgrey",
    linewidth = 0.4,
    show.legend = FALSE
  ) +
  geom_text(
    data = last_points,
    aes(
      x = label_x,
      y = label_y,
      label = country
    ),
    hjust = 0,
    size = 5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = aqli_palette) +
  scale_x_continuous(
    breaks = seq(1998, 2024, by = 2)
  ) +
  scale_y_continuous(
    limits = c(0, y_max + 2),
    breaks = seq(0, y_max + 2, by = 2),
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Year",
    y = expression("Annual Average " ~ PM[2.5] ~ " Concentration (in µg/m³)")
  ) +
  ggthemes::theme_clean() +
  themes_aqli_base +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 120, 20, 20),
    axis.title.y = element_text(size = 20, margin = margin(r = 0.6, unit = "cm"), color = "#222222"),
    axis.title.x = element_text(size = 20, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color = "#222222"),
    axis.line = element_line(),
    axis.text = element_text(size = 20, color = "#222222"),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(color = "white", fill = "white")
  )

# ------------------------------------------------------------------------------
# Export Plot
# ------------------------------------------------------------------------------
ggsave(
  filename = "~/Desktop/Final plots aqli/US and Canada/fig10.3/ar_us_canada_fig10.3.png",
  plot = ar_us_canada_fig10.3,
  width = 14,
  height = 8,
  dpi = 300,
  bg = "white"
)
ggsave(
  filename = "~/Desktop/Final plots aqli/US and Canada/fig10.3/ar_us_canada_fig10.3.svg",
  plot = ar_us_canada_fig10.3,
  width = 14,
  height = 8,
  dpi = 300,
  bg = "white"
)