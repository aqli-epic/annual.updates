################################################################################
# Ecuador Factsheet – Figure 2: Top 10 Threats to Life Expectancy
################################################################################

# --- 1. Load required helper functions ----------------------------------------

source("~/R/july.2025.helper.script.R")


# --- 2. Filter GBD dataset for Ecuador's leading risk factors -----------------

# Define causes to include in the comparison
selected_causes <- c(
  "Ambient ozone pollution", "Child and maternal malnutrition", "Childhood sexual abuse and bullying",
  "Dietary risks", "Drug use", "HIV/AIDS and sexually transmitted infections", "High alcohol use",
  "Intimate partner violence", "Low physical activity", "Neglected tropical diseases and malaria",
  "Nutritional deficiencies", "Occupational risks", "PM2.5 relative to WHO guideline",
  "Self-harm and interpersonal violence", "Substance use disorders", "Tobacco", "Transport injuries",
  "Non-optimal temperature", "Other environmental risks", "Unintentional injuries",
  "Unsafe sex", "Unsafe water, sanitation, and handwashing"
)

# Filter and get top 10 based on life years lost
ecuador_fig2_data <- gbd_results_master_2025 %>%
  filter(
    country == "Ecuador",
    cause_of_death %in% selected_causes
  ) %>%
  slice_max(lyl, n = 10)

# Rename column for consistency with AQLI functions
colnames(ecuador_fig2_data)[3] <- "llpp_who_2023"

# Apply AQLI-style color bucket classification
ecuador_fig2_data <- ecuador_fig2_data %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2023")


# --- 3. Plot: Figure 2 – Top 10 Life Expectancy Threats -----------------------

ecuador_fs_fig2 <- ecuador_fig2_data %>%
  ggplot() +
  geom_col(
    aes(
      x = forcats::fct_reorder(cause_of_death, llpp_who_2023),
      y = llpp_who_2023,
      fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)
    ),
    width = 0.5
  ) +
  
  # Axis labels and titles
  labs(
    x = "Threats to Life Expectancy",
    y = "Life Years Lost",
    fill = "Life years lost",
    title = ""
  ) +
  
  coord_flip() +  # Flip coordinates for horizontal bar chart
  
  # Apply Tufte-style and AQLI base theme
  ggthemes::theme_tufte() +
  themes_aqli_base +
  
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 24, color = "#222222"),
    legend.title = element_text(size = 24, color = "#222222"),
    axis.text = element_text(size = 20, color = "#222222"),
    axis.title.y = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"), color = "#222222"),
    axis.title.x = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color = "#222222"),
    axis.ticks.y = element_blank(),
    axis.line = element_line(),
    panel.grid.major.y = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    legend.box.background = element_rect(color = "black"),
    plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 0.8, unit = "cm")),
    plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 0.8, unit = "cm")),
    plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 0.8, unit = "cm"))
  ) +
  
  # Y-axis range and tick marks
  scale_y_continuous(breaks = seq(0, 2, 1), limits = c(0, 2)) +
  
  # Custom color scale based on AQLI buckets
  scale_fill_manual(values = c(
    "0 to < 0.1" = "#FFFFFF", 
    "0.1 to < 0.5" = "#FFF2E1", 
    "0.5 to < 1" = "#FFEDD3", 
    "1 to < 2" = "#FFC97A", 
    "2 to < 3" = "#FFA521", 
    "3 to < 4" = "#EB6C2A", 
    "4 to < 5" = "#D63333", 
    "5 to < 6" = "#8E2946", 
    ">= 6" = "#451F59"
  )) +
  
  # Legend layout
  guides(fill = guide_legend(nrow = 1))

