# ------------------------------------------------------------------------------
# AQLI 2026 – Regional PM2.5 Life Expectancy Burden Analysis
# Description : Computes population-weighted PM2.5 exposure and translates it 
#               into Life Years Lost (LYL) using WHO benchmarks. Produces a 
#               dual-axis visualization showing absolute LYL and regional share 
#               of global LYL.
#
# Developed By: Purushottam Gupta
# Role        : Data Architect / Data Analyst, AQLI (University of Chicago)
# Contact     : guptap@uchicago.edu
# Dependencies: dplyr, ggplot2, ggthemes, stringr, forcats, scales
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Load Dependencies & Source Files
# ------------------------------------------------------------------------------
source("~/R/helper_function_2026.R")
#### Dynamic Column Select ####

# Identify relevant columns dynamically:
# - PM2.5 concentrations (pmYYYY)
# - Life years lost relative to WHO and national standards
value_cols <- grep(
  "^(pm\\d{4}|llpp_who_\\d{4}|llpp_nat_\\d{4})$",
  names(gadm0_aqli_2024),
  value = TRUE
)


# Aggregate to region level using population-weighted averages
# Ensures estimates reflect population weighted rather than simple means
region_aqli <- gadm2_aqli_2024 %>%
  group_by(region, whostandard) %>%
  summarise(
    across(
      all_of(value_cols),
      ~ round(weighted.mean(.x, population, na.rm = TRUE), 2)
    ),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(region, population, whostandard, starts_with("pm"))


#### WHO-based LYL Compute ####

# Convert PM2.5 exposure into Life Years Lost (LYL)
# Using AQLI conversion factor (0.098 years per µg/m³ above WHO guideline)
gadm_aqli_region <- region_aqli %>%
  mutate(
    across(
      starts_with("pm"),
      ~ if_else(.x > whostandard,
                round(0.098 * (.x - whostandard), 4),
                0),
      .names = "llpp_who_{str_sub(.col, 3, 6)}"
    )
  )


#### Share of Global LYL ######

# Compute total LYL burden and each region's share of global LYL
gadm_aqli_region <- gadm_aqli_region %>%
  mutate(
    total_lyl = llpp_who_2024 * population,
    perc_lyl  = total_lyl / sum(total_lyl, na.rm = TRUE) * 100
  )

######## Scaling Factor #######

# Scale percentage values to align with LYL values for dual-axis plotting
scale_factor <- max(gadm_aqli_region$llpp_who_2024, na.rm = TRUE) /
  max(gadm_aqli_region$perc_lyl, na.rm = TRUE)


######## Data Ordering ########
# Remove Oceania (if needed) and order regions by LYL share
gadm_aqli_region <- gadm_aqli_region %>%
  filter(region != "Oceania") %>%
  mutate(
    region = fct_reorder(factor(region), perc_lyl)
  )


########## Visualization ######
# Dual-axis bar chart:
# - Left axis: absolute Life Years Lost
# - Right axis: share of global Life Years Lost
ar_regional_fig2.1 <- ggplot(gadm_aqli_region, aes(x = region)) +
  
  # Life Years Lost (primary axis)
  geom_col(
    aes(y = llpp_who_2024, fill = "Life Years Lost"),
    width = 0.4,
    position = position_nudge(x = -0.2)
  ) +
  
  # Share of Global LYL (secondary axis, scaled)
  geom_col(
    aes(y = perc_lyl * scale_factor,
        fill = "Share of Global Life Years Lost (%)"),
    width = 0.4,
    position = position_nudge(x = 0.2)
  ) +
  
  scale_y_continuous(
    name = "Life Years Lost",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Share of Global Life Years Lost (%)"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Life Years Lost" = "#E67E22",
      "Share of Global Life Years Lost (%)" = "#0B3C5D"
    )
  ) +
  
  scale_x_discrete(labels = label_wrap(15)) +
  
  labs(
    x = NULL,
    fill = NULL
  ) +
  
  ggthemes::theme_clean(base_size = 14) +
  
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 14, color = "#222222"),
    axis.title = element_text(size = 15, color = "#222222"),
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.text = element_text(size = 13),
    legend.box.background = element_rect(color = "black"),
    panel.grid.major.y = element_blank(),
    axis.line = element_line()
  )

###############################
######## End Here Plot ##########
###############################
