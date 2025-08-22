# Read the helper file
source("~/Downloads/annual.updates/R/july.2025.helper.script.R")

# Load the forest fire data
total_forest_fire_data_country <- read.csv("~/Downloads/total_forest_fire_data_country.csv", check.names = FALSE)

# Filter for the selected countries
selected_countries <- c("United States", "Canada")
df_filtered <- total_forest_fire_data_country[total_forest_fire_data_country$country %in% selected_countries, ]

# Check the column names to ensure we are targeting the correct year columns
print(colnames(df_filtered))

# Pivot data from wide to long format, selecting the years dynamically
df_new <- df_filtered %>% 
  # Using select and starts_with to ensure we target the correct year columns
  select(country, starts_with("20")) %>%
  pivot_longer(cols = -country, names_to = "Year", values_to = "Area_burnt")

# Remove the 'x' prefix from the 'Year' column (if necessary) and convert it to numeric
df_new$Year <- gsub("^x", "", df_new$Year)
df_new$Year <- as.numeric(df_new$Year)
# Create the plot
forest_fire_US_Can <- ggplot(data = df_new, aes(x = Year, y = Area_burnt / 10^6, color = country, group = country)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(0, 3, 0.5), limits = c(0, 3)) +
  scale_x_continuous(breaks = c(seq(2002, 2020, 3), 2023)) +
  labs(title = "",
       x = "Year",
       caption = "Global Wildfire Information System GWIS (https://gwis.jrc.ec.europa.eu/apps/country.profile/downloads)
GWIS derives wildfire events and burnt areas using the GlobFire methodology, based on the MODIS burnt area product (MCD64A1)",
       y = "Area burnt (million Hectares)",
       color = "country") +
  theme_minimal() +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e")) + # Distinct colors for light/dark themes
  theme(legend.position = "bottom",
        axis.title.y = element_text(size = 13, margin = margin(r = 0.6, unit = "cm")),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 13, margin = margin(t = 0.6, b = 0.6, unit = "cm")),
        axis.line = element_line(),
        legend.text = element_text(size = 11),
        legend.box.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 0.7, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 8, face = "italic"),
        plot.caption = element_text(size = 14, margin = margin(t = 0.8, unit = "cm"), hjust = 0, face = "italic"),
        axis.text = element_text(size = 12),
        plot.background = element_rect(color = "white"),
        axis.ticks = element_blank(),
        legend.key.width = unit(2, "cm"),
        panel.grid = element_blank())