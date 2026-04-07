source("~/R/july.2026.helper.script.R")

tree_cover_loss_from_fires__ha <- read.csv("~/treecover_loss_from_fires_global.csv")

# =========================
# 1) GLOBAL TREE COVER LOSS (wide -> long)
# =========================
tree_loss_global <- tree_cover_loss_from_fires__ha %>%
  select(year, Global) %>%   # <-- change 'Global' if column name differs
  mutate( year = as.integer(year), tcl_fire_ha = Global) %>%
  select(year, tcl_fire_ha)

tree_loss_plot_global_bar <- ggplot(tree_loss_global, aes(x = year, y = tcl_fire_ha)) +
  geom_col(fill = "#D2693C", width = 0.7) +
  scale_y_continuous(name = "Global tree cover loss from fires (million hectares)",breaks = seq(0, 15, 1),limits = c(0, 15)) +
  scale_x_continuous(name = "Year",breaks = seq(2002, 2024, 2)) +
  themes_aqli_base +
  theme(
    axis.text = element_text(size = 20, color = "#222222"),
    axis.title.y = element_text(size = 24,margin = margin(r = 0.6, unit = "cm"),color = "#222222"),
    axis.title.x = element_text( size = 24,margin = margin(r = 0.6, unit = "cm"),color = "#222222"),
    axis.line = element_line(color = "#222222", linewidth = 1.1))