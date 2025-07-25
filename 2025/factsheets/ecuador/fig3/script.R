###############################################################################
#   Ecuador Factsheet – Figure 3: Potential Life‑Expectancy Gain by Province  #
###############################################################################

# --- 1. Load required helper functions ----------------------------------------

source("~/R/july.2025.helper.script.R")

# --- 1.  Filter Ecuador rows from the master GADM‑AQ data --------------------

ecuador_aqli_2023 <- gadm2_aqli_2023 %>%            # master dataset
  filter(country == "Ecuador")                      # keep only Ecuador 


# --- 2.  Build the Figure‑3 dataset  -----------------------------------------

# Notes on new names:
#   * avg_pm25_2023      – population‑weighted average PM2.5 in 2023
#   * le_gain            – potential life‑expectancy gain if WHO guideline met
#   * llpp_who_2023_avg  – population‑weighted life‑years lost (AQLI metric)

ecuador_fs_fig3_dataset <- ecuador_aqli_2023 %>%
  select(country, name_1, name_2, population, pm2023, llpp_who_2023) %>%
  group_by(name_1) %>%                                # province level
  mutate(
    pop_weights               = population / sum(population, na.rm = TRUE),
    pm2023_weighted           = pop_weights * pm2023,
    llpp_who_2023_weighted    = pop_weights * llpp_who_2023
  ) %>%
  summarise(
    total_population   = sum(population, na.rm = TRUE),
    avg_pm25_2023      = sum(pm2023_weighted, na.rm = TRUE),
    llpp_who_2023_avg  = sum(llpp_who_2023_weighted, na.rm = TRUE),
    .groups            = "drop"
  ) %>%
  mutate(
    # Translate excess PM2.5 into life‑expectancy gain
    le_gain = pmax((avg_pm25_2023 - who_guideline) * le_constant, 0)
  ) %>%
  slice_max(total_population, n = 10) %>%             # top‑10 most‑populous provinces
  add_aqli_color_scale_buckets("lyl", "llpp_who_2023_avg")   # custom helper


# --- 3.  Draw Figure 3 -------------------------------------------------------

ecuador_fs_fig3 <- ecuador_fs_fig3_dataset %>%
  ggplot() +
  geom_col(
    aes(x = reorder(name_1, llpp_who_2023_avg),
        y = llpp_who_2023_avg,
        fill = lyl_bucket),
    width = 0.5
  ) +
  coord_flip() +                                     # horizontal bars
  
  # Axis & legend labels
  labs(
    x    = "Province",
    y    = "Potential Gain in Life Expectancy (Years)",
    fill = "Potential Gain in Life Expectancy (Years)"
  ) +
  
  # Y‑axis scale (0–2 years in 1‑year steps)
  scale_y_continuous(breaks = seq(0, 2, 1), limits = c(0, 2)) +
  
  # Manual fill palette (same classes as add_aqli_color_scale_buckets)
  scale_fill_manual(values = c(
    "0 to < 0.1" = "#FFFFFF",
    "0.1 to < 0.5" = "#FFF2E1",
    "0.5 to < 1"   = "#FFEDD3",
    "1 to < 2"     = "#FFC97A",
    "2 to < 3"     = "#FFA521",
    "3 to < 4"     = "#EB6C2A",
    "4 to < 5"     = "#D63333",
    "5 to < 6"     = "#8E2946",
    ">= 6"         = "#451F59"
  )) +
  
  ggthemes::theme_tufte() +                          # minimal theme
  theme(
    legend.position   = "bottom",
    legend.text       = element_text(size = 24, color = "#222222"),
    legend.title      = element_text(size = 24, color = "#222222"),
    plot.title        = element_text(hjust = 0.5, size = 16),
    plot.subtitle     = element_text(hjust = 0.5, size = 10, face = "italic",
                                     margin = margin(b = 0.8, unit = "cm")),
    plot.caption      = element_text(size = 8, hjust = 0, face = "italic"),
    legend.box.background = element_rect(color = "black"),
    plot.background   = element_rect(color = "white"),
    axis.line         = element_line(),
    axis.text         = element_text(size = 20, color = "#222222"),
    axis.title        = element_text(size = 24, color = "#222222"),
    axis.title.y      = element_text(margin = margin(r = 0.7, unit = "cm")),
    axis.title.x      = element_text(margin = margin(t = 0.6, b = 0.6, unit = "cm")),
    axis.ticks        = element_blank()
  )


# --- 4.  Export high‑resolution PNG ------------------------------------------

ggsave("fig3.png", ecuador_fs_fig3, width = 15, height = 10, dpi = 300)

ggsave("fig3.svg", ecuador_fs_fig3, width = 15, height = 10, dpi = 300)
