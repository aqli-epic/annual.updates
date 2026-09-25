# ==================== REQUIRED LIBRARIES ====================
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(openaq)

API_KEY <- "xxxxxxxxxxxxxxxx"

# ==================== 1. GET LOCATIONS IN INDIA (PM2.5) ====================
locations <- list_locations(
  limit         = 1000,
  parameters_id = 2,      # PM2.5
  countries_id  = c(166, 109),    # India
  rate_limit    = TRUE
  )

# Extract location data with country info
loc_df <- as_tibble(locations) %>%
  select(location_id = id,
         location_name = name,
         countries_id,
         country_name = country_name,   # adjust column name if needed
         latitude, longitude) %>%
  distinct()

cat("Found", nrow(loc_df), "locations in GAMBIA with PM2.5\n")

# Create lookup for country_id by location_id
country_lookup <- loc_df %>%
  select(location_id, countries_id)

# ==================== 2. GET SENSORS FOR EACH LOCATION ====================
get_sensors <- function(loc_id) {
  url <- paste0("https://api.openaq.org/v3/locations/", loc_id, "/sensors")

  res <- GET(url, add_headers(
    accept = "application/json",
    `X-API-Key` = API_KEY
  ))

  if (status_code(res) != 200) {
    message("❌ Sensors failed for location: ", loc_id)
    return(NULL)
  }

  data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

  if (length(data$results) == 0) return(NULL)

  df <- as_tibble(data$results)
  df$location_id <- loc_id

  return(df)
}

all_sensors <- map_dfr(loc_df$location_id, function(x) {
  Sys.sleep(0.25)
  cat("Sensors for location:", x, "\n")
  get_sensors(x)
})

sensor_ids <- unique(all_sensors$id)

cat("Total unique PM2.5 sensors:", length(sensor_ids), "\n")

# ==================== 3. GET DAILY MEASUREMENTS ====================
get_all_measurements <- function(sensor_id) {
  page <- 1
  all_data <- list()

  repeat {
    url <- paste0("https://api.openaq.org/v3/sensors/", sensor_id,
                  "/measurements/daily?limit=100&page=", page)


    # url <- paste0("https://api.openaq.org/v3/sensors/", sensor_id,
    #               "/days/monthly?limit=100&page=", page)



    res <- GET(url, add_headers(
      accept = "application/json",
      `X-API-Key` = API_KEY
    ))

    if (status_code(res) != 200) {
      message("Measurement failed for sensor ", sensor_id)
      break
    }

    txt <- content(res, "text", encoding = "UTF-8")
    data <- fromJSON(txt, flatten = TRUE)

    if (length(data$results) == 0) {
      cat("✅ Finished sensor:", sensor_id, "\n")
      break
    }

    df <- as_tibble(data$results)
    df$sensor_id <- sensor_id

    all_data[[page]] <- df
    page <- page + 1
    Sys.sleep(0.25)
  }

  if (length(all_data) == 0) return(NULL)
  bind_rows(all_data)
}

# Fetch measurements
all_measurements_raw <- map_dfr(sensor_ids, get_all_measurements)

# ==================== 4. FINAL CLEANING & ADDING location_id + country_id ====================

# First, link sensor_id → location_id
sensor_to_location <- all_sensors %>%
  select(sensor_id = id, location_id)

all_measurements <- all_measurements_raw %>%
  left_join(sensor_to_location, by = "sensor_id") %>%
  left_join(country_lookup, by = "location_id") %>%
  filter(parameter.name == "pm25" | parameter.id == 2) %>%   # ensure only PM2.5
  select(
    location_id,
    countries_id,
    sensor_id,
    # Add other useful columns you want
    date = period.datetimeFrom.utc,   # adjust column name based on actual structure
    value,
    parameter.name,
    parameter.units,
    everything()
  ) %>%
  arrange(location_id, sensor_id, date)

locations <- locations %>% select(id, name, country_name,  "latitude", "longitude",
                                  "owner_name", "providers_id", "provider_name")


all_measurements_lds <- all_measurements %>% select("location_id", "value", "sensor_id", "date", coverage.percentCoverage, coverage.percentComplete)
all_measurements <- left_join(all_measurements_lds, locations, by = c("location_id" = "id"))
# View result
glimpse(all_measurements)
library(lubridate)
all_measurements$date <- ymd_hms(all_measurements$date)

# Extract year and month
all_measurements$year <- year(all_measurements$date)
all_measurements$month <- month(all_measurements$date)

###############
after_test <- read.csv("~/Desktop/paki_gam.csv")
after_test <- unique(after_test)
check <- after_test %>% group_by(year, month, location_id, owner_name, name, country_name, latitude, longitude) %>% summarise(count =n(),
                                                                                   pm25 = mean(value))

check1 <- all_measurements %>% group_by(year, month, name, country_name) %>% summarise(count =n(),
                                                                                                                                    pm25 = mean(value))

# Save the final file
write.csv(all_measurements, "openaq_pm25_india_daily_with_location_country.csv",
          row.names = FALSE)

cat("Final dataframe has", nrow(all_measurements), "rows with location_id and country_id added.\n")


########################################################
# library(httr)
# library(jsonlite)
# library(dplyr)
#
# API_KEY <- "YOUR_API_KEY"
#
# get_locations <- function(country_id = 166) {
#
#   url <- "https://api.openaq.org/v3/locations"
#
#   res <- GET(
#     url,
#     query = list(
#       countries_id = country_id,
#       limit = 1000,
#       page = 1
#     ),
#     add_headers(`X-API-Key` = API_KEY)
#   )
#
#   data <- fromJSON(content(res, "text", encoding = "UTF-8"))
#
#   data$results
# }
#
# get_locations()
#
# locations <- get_locations()
#
# df <- locations %>%
#   transmute(
#     location_id = id,
#     location_name = name,
#     city = locality,
#     state = administrative_area_level_1,
#     district = administrative_area_level_2,
#     country = country$name,
#     lat = coordinates$latitude,
#     lon = coordinates$longitude
#   )
#


# library(dplyr)
#
# # Step 1: unique coords
# coords_unique <- all_measurements %>%
#   distinct(latitude, longitude)
#
# # Step 2: reverse geocode ONLY these
# coords_unique <- coords_unique %>%
#   rowwise() %>%
#   mutate(loc = list(get_location_osm(latitude, longitude))) %>%
#   mutate(
#     city_real = loc$city,
#     district = loc$district,
#     state = loc$state
#   ) %>%
#   select(-loc) %>%
#   ungroup()
# coords_unique <- coords_unique %>%
#   rowwise() %>%
#   mutate(loc = list(get_location_osm(lat, lon))) %>%
#   mutate(
#     city_real = if (!is.null(loc)) loc$city else NA_character_,
#     district  = if (!is.null(loc)) loc$district else NA_character_,
#     state     = if (!is.null(loc)) loc$state else NA_character_
#   ) %>%
#   select(-loc) %>%
#   ungroup()
# # Step 3: join back
# df_final <- df %>%
#   left_join(coords_unique, by = c("lat", "lon"))
