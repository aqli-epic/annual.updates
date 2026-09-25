library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(tidytext)
library(tidyr)
library(tidyverse)
library(sf)
library(usethis)
library(devtools)
library(data.table)
library(svglite)
library(here)
library(shiny)
library(shinyjs)
library(bslib)
library(shinyWidgets)
library(reactable)
#library(reactablefmtr)
library(highr)
library(highcharter)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(shinycssloaders)
library(DT)
library(writexl)
library(arrow)
library(plotly)
#library(qs)
library(waiter)
library(shinyBS)
library(shinymanager)
library(reactable)
library(leafgl)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearth)


#source("~/Desktop/My AQLI Work/AQLI 2026/Updated plots code/helper_function_2026.R")
aqli_gadm0 <- read_csv("~/Desktop/AQFUND/data/gadm0_2024_narrow.csv")

# -----------------------------
# LOAD NATURAL EARTH DATA
# -----------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")
#states_all <- ne_states(returnclass = "sf")

# -----------------------------
# LOAD DATA
# -----------------------------
your_data <- read.csv("~/Desktop/AQFUND/data/measurement_openaq.csv")
your_data <- unique(your_data)
data_clean <- your_data %>% mutate(date = as.Date(paste(year, month, "01", sep = "-")))
data_clean$pm25 <- data_clean$value
data_clean$country_name <- data_clean$country_name %>%
  str_replace_all("\\u00A0", " ") %>%  # replace non-breaking space
  str_trim() %>%                       # remove leading/trailing spaces
  str_squish()  

data_clean$country_name[data_clean$country_name == "The Gambia"] <- "Gambia"


df <- data_clean[, c(
  "location_id",
  "sensor_id",
  "name",
  "country_name",
  "latitude",
  "longitude",
  "owner_name",
  "providers_id",
  "provider_name",
  "date",
  "year",
  "month",
  "pm25",
  "coverage.percentCoverage",
  "coverage.percentComplete"
)]

monthly_value <- data_clean %>%
  group_by( country_name,
           year, month) %>%
  summarise(pm25_avg = mean(pm25, na.rm = TRUE), .groups = "drop") %>%
  filter(year %in% c(2025, 2026)) %>%
  mutate(month_name = month.abb[month])

#################

monthly_value <- monthly_value %>%
  mutate(month_name = factor(month_name, levels = month.abb))

# Create series list (one line per year)
# series_list <- monthly_value %>%
#   arrange(year, month) %>%
#   group_by(year) %>%
#   summarise(data = list(pm25_avg), .groups = "drop") %>%
#   mutate(name = as.character(year)) %>%
#   list_parse()
# 
# # Plot
# highchart() %>%
#   hc_chart(type = "line") %>%
#   hc_title(text = "Monthly PM2.5 Trend") %>%
#   hc_xAxis(categories = month.abb, title = list(text = "Month")) %>%
#   hc_yAxis(title = list(text = "PM2.5")) %>%
#   hc_add_series_list(series_list) %>%
#   hc_tooltip(shared = TRUE, valueDecimals = 2) %>%
#   hc_plotOptions(
#     line = list(marker = list(enabled = TRUE))
#   )

####
# library(tidygeocoder)
# 
# df <- data.frame(
#   lat = 34.00585,
#   lon = 71.53775 
#   
# )
# 
# res <- data_clean %>%
#   reverse_geocode(
#     lat = latitude,
#     long = longitude,
#     method = "osm",
#     full_results = TRUE,
#     custom_query = list(`accept-language` = "en")
#   )


########
aqli_gadm0_filter <- aqli_gadm0 %>% filter(country %in% c("Pakistan", "Gambia"))
# -----------------------------
# AGGREGATIONS
# -----------------------------
country_trend <- data_clean %>%
  group_by(country_name, year) %>%
  summarise(pm25 = mean(pm25, na.rm = TRUE),
            monitors = n_distinct(location_id),
            .groups = "drop") %>% as.data.frame()




country_trend$country_name <- country_trend$country_name %>%
  str_replace_all("\\u00A0", " ") %>%  # replace non-breaking space
  str_trim() %>%                       # remove leading/trailing spaces
  str_squish()  

country_trend$country_name[country_trend$country_name == "The Gambia"] <- "Gambia"
############# TO be combined Data ###############
country_trend_comb  <-  country_trend %>% filter(year %in% c("2025", "2026")) %>% 
                                 select(country_name, year, pm25) %>% rename(country= country_name,
                                                                             pm = pm25) %>% mutate(
                                                                            natstandard = NA_integer_   
                                                                             )
aqli_gadm0_filter_final <- aqli_gadm0_filter %>% select("country", "year", "pm", "natstandard")

final_data <- bind_rows(country_trend_comb, aqli_gadm0_filter_final)

final_data$country <- trimws(final_data$country)

final_data$year <- as.integer(final_data$year)

final_data$natstandard[final_data$country=="Pakistan"] <- 15
final_data$whostd <- 5

city_trend <- data_clean %>%
  group_by(country_name, name, year) %>%
  summarise(pm25 = mean(pm25, na.rm = TRUE),
            monitors = n_distinct(location_id),
            .groups = "drop")

map_city <- data_clean %>%
  group_by(country_name, name) %>%
  summarise(pm25 = mean(pm25, na.rm = TRUE),
            lat = mean(latitude, na.rm = TRUE),
            lon = mean(longitude, na.rm = TRUE),
            monitors = n_distinct(location_id),
            .groups = "drop")

map_city$country_name <- map_city$country_name %>%
  str_replace_all("\\u00A0", " ") %>%  # replace non-breaking space
  str_trim() %>%                       # remove leading/trailing spaces
  str_squish()

map_city$country_name[map_city$country_name == "The Gambia"] <- "Gambia"

