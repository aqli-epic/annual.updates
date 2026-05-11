output$city_ui <- renderUI({
  cities <- data_clean %>%
    filter(country_name == input$country) %>%
    pull(name) %>% unique()
  
  selectInput("city", "Select City", choices = cities)
})

# -----------------------------
# FILTERED DATA
# -----------------------------
filtered_data <- reactive({
  data_clean %>%
    filter(country_name == input$country,
           year >= input$year_range[1],
           year <= input$year_range[2])
})

# -----------------------------
# VALUE BOXES
# -----------------------------
# output$avgPM <- renderValueBox({
#   valueBox(
#     round(mean(filtered_data()$pm25, na.rm = TRUE),1),
#     "Average PM2.5",
#     icon = icon("smog"),
#     color = "maroon"
#   )
# })
# 
# output$monitors <- renderValueBox({
#   valueBox(
#     n_distinct(filtered_data()$location_id),
#     "Monitoring Stations",
#     icon = icon("microchip"),
#     color = "primary"
#   )
# })
# 
# output$cities <- renderValueBox({
#   valueBox(
#     n_distinct(filtered_data()$name),
#     "Cities Covered",
#     icon = icon("city"),
#     color = "olive"
#   )
# })
# 
# output$trend <- renderValueBox({
#   df <- filtered_data() %>%
#     group_by(year) %>%
#     summarise(pm25 = mean(pm25), .groups = "drop")
#   
#   change <- tail(df$pm25,1) - head(df$pm25,1)
#   
#   valueBox(
#     paste0(round(change,1), " µg/m³"),
#     "PM2.5 Change",
#     icon = icon("chart-line"),
#     color = ifelse(change < 0, "success", "danger")
#   )
# })

# -----------------------------
# MAP (WORLD + STATES + CITIES)
# -----------------------------
output$map <- renderLeaflet({
  
  df <- map_city %>% filter(country_name == input$country)
  
  req(nrow(df) > 0)
  
  # Filter states for selected country (IMPORTANT optimization)
  states_sel <- world %>%
    filter(admin == input$country)
  
  pal <- colorNumeric(
    c("green","yellow","orange","red"),
    df$pm25
  )
  
  leaflet() %>%
    addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google',
             options = providerTileOptions(opacity = 0.5, minzoom =1, maxzoom = 15)) %>%
    
    # 🌍 World boundary (light)
    addPolygons(
      data = world,
      fill = FALSE,
      weight = 0.5,
      color = "#999999",
      opacity = 0.3
    ) %>%
    
    # 🏛️ States / provinces
    addPolygons(
      data = states_sel,
      fill = FALSE,
      color = "#444444",
      weight = 1,
      opacity = 0.7
    ) %>%
    
    # 📍 City points
    addCircleMarkers(
      data = df,
      lng = ~lon,
      lat = ~lat,
      radius = 7,
      fillColor = ~pal(pm25),
      fillOpacity = 0.9,
      color = "white",
      weight = 1,
      clusterOptions = markerClusterOptions(),
      label = ~lapply(paste0(
        "<b>", name, "</b><br>",
        "PM2.5: ", round(pm25,1), " µg/m³<br>",
        "Monitors: ", monitors
      ), HTML)
    ) %>%
    
    addLegend(
      pal = pal,
      values = df$pm25,
      title = "PM2.5 (µg/m³)"
    )
})

# -----------------------------
# COUNTRY TREND
# -----------------------------
output$countryPlot <- renderHighchart({
  
  df <- country_trend %>%
    filter(country_name == input$country) %>% filter(year %in% input$year_range)
  
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = paste("Air Quality Trend -", input$country)) %>%
    hc_xAxis(categories = df$year) %>%
    hc_yAxis(title = list(text = "PM2.5 (µg/m³)")) %>%
    hc_add_series(name = "PM2.5", data = df$pm25, color = "#800000") %>%
    hc_add_series(
      name = "WHO Guideline",
      data = rep(5, nrow(df)),
      dashStyle = "ShortDash",
      color = "black"
    )
})

# -----------------------------
# CITY TREND
# -----------------------------
output$cityPlot <- renderHighchart({
  
  df <- city_trend %>%
    filter(country_name == input$country,
           year >= input$year_range[1],
           year <= input$year_range[2])
  
  validate(
    need(nrow(df) > 0, "No data available for selected filters")
  )
  
  city_list <- split(df, df$name)
  
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = "City Comparison") %>%
    hc_xAxis(categories = sort(unique(df$year))) %>%
    hc_yAxis(title = list(text = "PM2.5 (µg/m³)")) %>%
    
    hc_add_series_list(
      lapply(city_list, function(x) {
        list(
          name = unique(x$name),
          data = x$pm25,
          type = "line"
        )
      })
    ) %>%
    hc_legend(enabled = TRUE)
})


###################################################################### 
#

# output$line_pm <- renderHighchart({
#   
#   data_df <- final_data %>%
#     filter(country %in% input$country) %>%
#     arrange(year)
#   
#   text <- paste0(
#     "Annual <span style='color:maroon;'>",
#     unique(data_df$country),
#     "</span> PM<sub>2.5</sub> Concentration"
#   )
#   
#   # ---- ensure numeric x-axis ----
#   data_df <- data_df %>%
#     mutate(year = as.numeric(year))
#   
#   # ---- Highchart ----
#   highchart() %>%
#     
#     hc_chart(type = "line") %>%
#     
#     # ================= X AXIS WITH BREAK =================
#   hc_xAxis(
#     type = "linear",
#     title = list(text = "Year"),
#     
#     breaks = list(
#       list(
#         from = 2024,
#         to = 2025,     # 👈 gap region
#         breakSize = 1  # controls visual spacing
#       )
#     ),
#     
#     plotLines = list(
#       list(
#         color = "maroon",
#         width = 1.5,
#         value = 2024,
#         dashStyle = "Dash",
#         zIndex = 5,
#         
#         label = list(
#           text = "Post-2024:<br>AQ Fund monitoring<br>begins",
#           useHTML = TRUE,
#           align = "left",
#           y = 25,
#           x = -120,
#           style = list(
#             color = "maroon",
#             fontWeight = "bold",
#             fontSize = "11px"
#           )
#         )
#       )
#     )
#   ) %>%
#     
#     # ================= Y AXIS =================
#   hc_yAxis(
#     title = list(text = "PM2.5 (µg/m³)")
#   ) %>%
#     
#     # ================= PM2.5 SERIES =================
#   hc_add_series(
#     name = "PM2.5",
#     data = lapply(1:nrow(data_df), function(i) {
#       list(x = data_df$year[i], y = data_df$pm[i])
#     }),
#     type = "spline",
#     color = "#1f77b4"
#   ) %>%
#     
#     # ================= STANDARD LINE =================
#   hc_add_series(
#     name = "PM2.5 National Standard",
#     data = lapply(1:nrow(data_df), function(i) {
#       list(x = data_df$year[i], y = data_df$natstandard[i])
#     }),
#     type = "line",
#     dashStyle = "Dash",
#     color = "grey"
#   ) %>%
#     
#     # ================= TITLE =================
#   hc_title(text = text, useHTML = TRUE) %>%
#     
#     hc_subtitle(
#       text = paste0(
#         "National Avg. PM2.5 Standard (",
#         unique(data_df$natstandard),
#         " µg/m³)"
#       )
#     ) %>%
#     
#     hc_tooltip(
#       shared = TRUE,
#       crosshairs = TRUE
#     )
# })

############################################# End 

output$line_pm <- renderHighchart({
  
  data_df <- final_data %>%
    filter(country %in% input$country) %>%
    arrange(year) %>%
    mutate(year = as.numeric(year))
  
  text <- paste0(
    "Annual <span style='color:maroon;'>",
    unique(data_df$country),
    "</span> PM<sub>2.5</sub> Concentration"
  )
  
  # -------------------------
  # SPLIT DATASETS
  # -------------------------
  
  satellite_df <- data_df %>%
    filter(year >= 1998, year <= 2024)
  
  ground_df <- data_df %>%
    filter(year >= 2025, year <= 2026)
  
  # -------------------------
  # HIGHCHART
  # -------------------------
  
  highchart() %>%
    
    hc_chart(type = "line") %>%
    
    # ================= X AXIS =================
  
  
  hc_xAxis(
    type = "linear",
    title = list(text = "Year"),
    
    plotLines = list(
      
      # ================= POST-2024 =================
      list(
        color = "#b45a5a", 
        width = 1.5,
        value = 2024,
        dashStyle = "Dash",
        zIndex = 5,
        
        label = list(
          text = "Post-2024:<br>AQ Fund<br>monitoring",
          useHTML = TRUE,          
          rotation = 0,   # 🔥 forces horizontal text

          align = "left",
          y = 25,
          x = -30,
          style = list(
            color = "#ff7f0e",
            fontWeight = "bold",
            fontSize = "11px"
          )
        )
      ),
      
      # ================= PRE-2024 ANNOTATION =================
      list(
        color = "transparent",
        width = 0,
        value = 2005,
        
        label = list(
          text = "1998-2024:<br>Satellite-based PM2.5 estimation",
          useHTML = TRUE,
          
          rotation = 0,   # 🔥 forces horizontal text
          
          style = list(
            color = "#1f77b4",
            fontWeight = "bold",
            fontSize = "11px",
            whiteSpace = "nowrap"   # prevents wrapping into vertical stacking
          ),
          
          y = 150,   # adjust vertical position
          x = 0
        )
      )    )
  ) %>%     
    # ================= Y AXIS =================
  hc_yAxis(
    title = list(text = "PM2.5 (µg/m³)")
  ) %>%
    
    # ================= SERIES 1: SATELLITE =================
  hc_add_series(
    name = "Satellite Data (1998–2024)",
    data = lapply(1:nrow(satellite_df), function(i) {
      list(x = satellite_df$year[i], y = satellite_df$pm[i])
    }),
    type = "spline",
    color = "#1f77b4",
    lineWidth = 2
  ) %>%
    
    # ================= SERIES 2: GROUND =================
  hc_add_series(
    name = "Ground Monitoring (2025–2026)",
    data = lapply(1:nrow(ground_df), function(i) {
      list(x = ground_df$year[i], y = ground_df$pm[i])
    }),
    type = "spline",
    color = "#ff7f0e",
    lineWidth = 2
  ) %>%
    
    # ================= NATIONAL STANDARD =================
  hc_add_series(
    name = "PM2.5 National Standard",
    data = lapply(1:nrow(data_df), function(i) {
      list(x = data_df$year[i], y = data_df$natstandard[i])
    }),
    type = "line",
    dashStyle = "ShortDash",
    color = "grey"
  ) %>%
  
    # ================= TITLE =================
  hc_title(text = text, useHTML = TRUE) %>%
    
    hc_subtitle(
      text = paste0(
        "National Avg. PM2.5 Standard (",
        unique(data_df$natstandard),
        " µg/m³)"
      )
    ) %>%
    
    hc_tooltip(
      shared = TRUE,
      crosshairs = TRUE
    )
})

output$monthly_pm25 <- renderHighchart({
  
  data_df <- monthly_value %>%
    filter(country_name %in% input$country) %>%
    mutate(
      year = as.character(year),
      month_num = match(month_name, month.name),
      month_short = month.abb[month_num]
    ) %>%
    arrange(year, month_num)
  
  text <- paste0(
    "Monthly <span style='color:maroon;'>",
    unique(data_df$country_name),
    "</span> PM<sub>2.5</sub> Concentration"
  )
  
  highchart() %>%
    
    hc_chart(type = "line") %>%
    
    hc_xAxis(
      categories = month.abb,
      title = list(
        text = "Month",
        style = list(fontWeight = "bold")
      ),
      labels = list(
        style = list(
          fontWeight = "bold",
          fontSize = "12px"
        )
      )
    ) %>%
    
    hc_yAxis(
      title = list(
        text = "PM2.5 (µg/m³)",
        style = list(fontWeight = "bold")
      ),
      labels = list(
        style = list(
          fontWeight = "bold",
          fontSize = "12px"
        )
      )
    ) %>%
    
    hc_title(
      text = text,
      useHTML = TRUE
    ) %>%
    
    hc_tooltip(
      shared = TRUE,
      crosshairs = TRUE,
      valueDecimals = 2
    ) %>%
    
    hc_plotOptions(
      series = list(
        marker = list(enabled = TRUE)
      )
    ) %>%
    
    hc_add_series(
      data = data_df %>%
        filter(year == unique(year)[1]) %>%
        pull(pm25_avg),
      name = unique(data_df$year)[1]
    ) %>%
    
    {
      p <- .
      
      yrs <- unique(data_df$year)
      
      if(length(yrs) > 1){
        
        for(i in 2:length(yrs)){
          
          p <- p %>%
            hc_add_series(
              data = data_df %>%
                filter(year == yrs[i]) %>%
                pull(pm25_avg),
              name = yrs[i]
            )
        }
      }
      
      p
    }
  
})

output$vb_countries <- renderText({ length(unique(data_clean$country_name)) })
output$vb_years <- renderText({ length(unique((data_clean %>% filter(country_name %in% input$country))$providers_id)) })
output$vb_avg_pm25 <- renderText({ data_clean %>% filter(country_name %in% input$country) %>%  pull(sensor_id) %>% unique() %>% length() })
output$vb_health_impact <- renderText({ "xxM" })

    


