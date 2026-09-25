# ============================================================
# Author:        Purushottam Gupta
# Organization:  Air Quality Life Index (AQLI)
#                University of Chicago
# Email:         guptap@uchicago.edu
#
# Description:
# Geographic Filtering for GIS Maps
# (Country → State → District)
# =========================================================

# Reactive containers to store selected geographic filters
rv_country  <- reactiveValues()   # selected country
rv_state    <- reactiveValues()   # selected state/province
rv_district <- reactiveValues()   # selected district


# ---------------------------------------------------------
# Initialize reactive values from UI inputs
# isolate() prevents triggering reactive updates on startup
# ---------------------------------------------------------

isolate({
  rv_country$country_gis  <- input$country_gis
  rv_state$level_1_id     <- input$level_1_id  
  rv_district$level_2_id  <- input$level_2_id
})


# ---------------------------------------------------------
# Update state list when country selection changes
# ---------------------------------------------------------

observeEvent(input$country_gis, {
  
  # Get available states for selected country
  states <- unique(
    select_filter[country %in% input$country_gis, name_1]
  )
  
  updatePickerInput(
    session,
    "level_1_id",
    choices = states,
    selected = states
  )
})


# ---------------------------------------------------------
# Update district list when state selection changes
# ---------------------------------------------------------

observeEvent(input$level_1_id, {
  
  # Get available districts for selected states
  districts <- unique(
    select_filter[name_1 %in% input$level_1_id, name_2]
  )
  
  updatePickerInput(
    session,
    "level_2_id",
    choices = districts,
    selected = districts
  )
})


# ---------------------------------------------------------
# Store updated filter selections in reactiveValues
# Only update when dropdown menus are not open
# (prevents UI resetting while user is interacting)
# ---------------------------------------------------------

observe({
  
  if(!isTRUE(input$city_open) &
     !isTRUE(input$level_1_id_open) &
     !isTRUE(input$level_2_id_open))
  {
    
    rv_country$country_gis <- input$country_gis
    rv_state$level_1_id    <- input$level_1_id  
    rv_district$level_2_id <- input$level_2_id
    
  }
})


# =========================================================
# Reactive datasets for GIS visualizations
# =========================================================

# ---------------------------------------------------------
# Country-level dataset
# ---------------------------------------------------------

get_country_df <- reactive({
  
  df_temp <- gadm0_long
  
  return(
    df_temp[country %in% rv_country$country_gis]
  )
  
}) %>% bindCache(rv_country$country_gis)


# ---------------------------------------------------------
# State / Province-level dataset
# ---------------------------------------------------------

get_state_df <- reactive({
  
  df_temp <- gadm1_long
  
  return(
    df_temp[
      country %in% rv_country$country_gis &
        name_1  %in% rv_state$level_1_id
    ]
  )
  
}) %>% bindCache(rv_country$country_gis, rv_state$level_1_id)


# ---------------------------------------------------------
# District-level dataset
# ---------------------------------------------------------

get_district_df <- reactive({
  
  df_temp <- gadm2_long
  
  return(
    df_temp[
      country %in% rv_country$country_gis &
        name_1  %in% rv_state$level_1_id &
        name_2  %in% rv_district$level_2_id
    ]
  )
  
}) %>% bindCache(
  rv_country$country_gis,
  rv_state$level_1_id,
  rv_district$level_2_id
)


#
font.size <- "8pt"

opts1 <- list(
  
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#008000', 'color': '#fff'});",
    
    paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
    
    "}"),
  
  searchHighlight = TRUE,
  # columnDefs = list(list(targets = c(1:10), searchable = FALSE)),
  pageLength = 10,
  lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')),
  # dom = 't',
  scrollX = TRUE,
  scrollY = 300,
  scroller = TRUE,
  fixedColumns = TRUE,
  # buttons = c('copy', 'csv', 'excel'),
  buttons = list(list(extend = c('excel'), filename= paste0("pm25_data_",Sys.time()))),
  
  dom = 'lfrtiBp'
  
)
# Render Leaflet map
# observeEvent(input$switch_gis, {
#   if (input$switch_gis) {
#     updateCheckboxInput(session, "switch_gis", label = "Hide Map")
#   } else {
#     updateCheckboxInput(session, "switch_gis", label = "Show Map")
#   }
# })

observeEvent(input$switch_gis, {

output$country_wise_pm_llp <- renderLeaflet({
  
  if (input$switch_btn == "pm25") {
    
    data_df <- gadm1_shp_pm %>%
      filter(name0 == input$country_gis) %>%
      select(name1, !!input$year_gis, population) 
    
    data_df$label_text <- sprintf(
      "<div style='font-family:sans-serif;font-size:13px;line-height:1.5;'>
         <b style='font-size:14px;color:#2c3e50;'>%s</b><br/>
         <span style='color:#7f8c8d;'>Population: </span>
         <b style='color:#2c3e50;'>%s</b><br/>
         
         <span style='color:#7f8c8d;'><b>PM<sub>2.5</sub> Concentration</b><br/></span>
         <b style='color:#e67e22;'>%.1f µg/m³</b>
       </div>",
      data_df$name1,
      formatC(data_df$population, format = "f", big.mark = ",", digits = 0),
      data_df[[input$year_gis]]
    )
    
    pal <- colorBin(
      palette = c("#b7ebf1", "#8fd8e4", "#3db1c8", "#3f8dac", "#416891", "#434475", "#451f59"),
      domain = data_df[[input$year_gis]],
      bins = c(0, 10, 25, 35, 50, 60, Inf),
      na.color = "grey"
    )
    
    leaflet(data_df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      addPolygons(
        layerId = ~name1,
        fillColor = ~pal(data_df[[input$year_gis]]),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = lapply(data_df$label_text, HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      
      addControl(
        html = HTML("
    <div style='padding: 8px; background: rgba(255,255,255,0.8); font-size: 12px; border-radius: 4px;'>
      <b>PM<sub>2.5</sub> Concentration (µg/m³)</b><br/>
      <div style='display: flex; flex-wrap: wrap; gap: 6px 10px; align-items: center; margin-top: 6px;'>
        <div style='display: flex; align-items: center; gap: 4px;'>
          <div style='background:#b7ebf1; width:20px; height:12px;'></div><span>0–10</span>
        </div>
        <div style='display: flex; align-items: center; gap: 4px;'>
          <div style='background:#8fd8e4; width:20px; height:12px;'></div><span>10–25</span>
        </div>
        <div style='display: flex; align-items: center; gap: 4px;'>
          <div style='background:#3db1c8; width:20px; height:12px;'></div><span>25–35</span>
        </div>
        <div style='display: flex; align-items: center; gap: 4px;'>
          <div style='background:#3f8dac; width:20px; height:12px;'></div><span>35–50</span>
        </div>
        <div style='display: flex; align-items: center; gap: 4px;'>
          <div style='background:#416891; width:20px; height:12px;'></div><span>50–60</span>
        </div>
        <div style='display: flex; align-items: center; gap: 4px;'>
          <div style='background:#434475; width:20px; height:12px;'></div><span>60+</span>
        </div>
      </div>
    </div>
  "),
        position = "bottomleft"
      ) %>%  addFullscreenControl(
        position = "topleft",      # Optional: position of the button (default is "topleft")
        pseudoFullscreen = FALSE   # Optional: if TRUE, fullscreen to page width/height, not true browser fullscreen
      )
    
    
  }
  
  
  else {
    data_df <- gadm1_shp_llppwho %>%
      filter(name0 == input$country_gis) %>%
      select(name1, !!input$year_gis, population) 
    
    data_df$label_text <- sprintf(
      "<div style='font-family:sans-serif;font-size:13px;line-height:1.5;'>
     <b style='font-size:14px;color:#2c3e50;'>%s</b><br/>
     <span style='color:#7f8c8d;'>Population: </span>
     <b style='color:#2c3e50;'>%s</b><br/>
     <span style='color:#7f8c8d;'>Life Expectancy Loss: </span>
     <b style='color:#e67e22;'>%.1f years</b>
     </div>",
      data_df$name1,
      formatC(data_df$population, format = "f", big.mark = ",", digits = 0),
      data_df[[input$year_gis]]
    )
    pal <- colorBin(
      palette = c("#ffedd3", "#ffc97a", "#ffa521", "#eb6c2a", "#d63333", "#8e2946"),
      domain = data_df[[input$year_gis]],
      bins = c(0, 1, 2, 3, 5, 10, Inf)
    )
    
    
    leaflet(data_df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      addPolygons(
        layerId = ~name1,
        fillColor = ~pal(data_df[[input$year_gis]]),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = lapply(data_df$label_text, HTML),
        
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      
      addControl(
        html = HTML("
    <div style='padding: 8px; background: rgba(255,255,255,0.85); font-size: 12px; border-radius: 4px; max-width: 100%;'>
      <b>Life Expectancy Loss (Years)</b><br/>
      <div style='display: flex; flex-wrap: wrap; gap: 8px; align-items: center; margin-top: 6px;'>
        <div style='display: flex; align-items: center; gap: 4px;'>
          <div style='background:#ffedd3; width:20px; height:12px;'></div><span>0–1</span>
        </div>
        <div style='display: flex; align-items: center; gap: 4px;'>
          <div style='background:#ffc97a; width:20px; height:12px;'></div><span>1–2</span>
        </div>
        <div style='display: flex; align-items: center; gap: 4px;'>
          <div style='background:#ffa521; width:20px; height:12px;'></div><span>2–3</span>
        </div>
        <div style='display: flex; align-items: center; gap: 4px;'>
          <div style='background:#eb6c2a; width:20px; height:12px;'></div><span>3–5</span>
        </div>
        <div style='display: flex; align-items: center; gap: 4px;'>
          <div style='background:#d63333; width:20px; height:12px;'></div><span>5–10</span>
        </div>
        <div style='display: flex; align-items: center; gap: 4px;'>
          <div style='background:#8e2946; width:20px; height:12px;'></div><span>10+</span>
        </div>
      </div>
    </div>
  "),
        position = "bottomleft"
      ) %>% addFullscreenControl(
        position = "topleft",      # Optional: position of the button (default is "topleft")
        pseudoFullscreen = FALSE   # Optional: if TRUE, fullscreen to page width/height, not true browser fullscreen
      )
    
    
    
  }
  
  ########## click events
  
  
  
})

})

# On click, show modal with chart + table
# Debug: Monitor click events



# Handle polygon click
observeEvent(input$country_wise_pm_llp_shape_click, {
  print("Map shape clicked!")
  click_id <- input$country_wise_pm_llp_shape_click$id
  if (is.null(click_id)) {
    print("No click ID detected")
    return()
  }
  
  state_id <- click_id
  if (input$switch_btn == "pm25") {
    
    
    # clicked_data <- gadm2_pm25 %>% filter(name0 == input$country_gis) %>%
    #   select(name0, name1,name2,population, !!input$year_gis) %>% filter() %>% filter(name1 == state_id) %>% 
    #   rename(Value = !!sym(input$year_gis)) %>% arrange(Value)
    
    
    clicked_data <- gadm2_pm25[
      name0 == input$country_gis & name1 == state_id,
      .(name0, name1, name2, population, Value = get(input$year_gis))
    ][order(Value)]
    
    
  }
  else{
    
    # clicked_data <-  gadm2_llppwho %>% filter(name0 == input$country_gis) %>%
    #   select(name0, name1,name2,population, !!input$year_gis) %>% filter() %>% filter(name1 == state_id) %>% 
    #   rename(Value = !!sym(input$year_gis)) %>% arrange(Value)
    
    
    clicked_data <- gadm2_llppwho[
      name0 == input$country_gis & name1 == state_id,
      .(name0, name1, name2, population, Value = get(input$year_gis))
    ][order(Value)]
    
    
  }
  
  
  

  
  if (nrow(clicked_data) == 0) {
    print("No data found for clicked state")
    showModal(modalDialog(
      title = paste("State", state_id, "Details"),
      "No data available for this state.",
      easyClose = TRUE,
      size = "l"
    ))
    return()
  }
  
  
  showModal(
    modalDialog(
      title = div(
        style = "font-size: 20px; font-weight: bold; color: #2c3e50;",
        paste0("Country : ",input$country_gis," → " ,"State : ", state_id)
      ),
      uiOutput("modal_content"),
      size = "l",
      easyClose = TRUE,
      fade = TRUE,
      footer = modalButton("Close"),
      class = "modal-balanced"
    )
  )
  
  
  
  output$modal_content <- renderUI({
    
    
    tabsetPanel(type = "tabs",
                tabPanel("Top 10 Most Polluted Region", highchartOutput("chart")%>% withSpinner(color="#0dc5c1")),
                # tabPanel("Formation", highchartOutput("cboformation1")%>% withSpinner(color="#0dc5c1")),
                tabPanel("Tabular Data", dataTableOutput("table")%>% withSpinner(color="#0dc5c1"))
    )
    
  })
  
  # color_map <- c(
  #   "0 - < 0.1 years" = "#FFFFFF",
  #   "0.1 - 0.5" = "#FFE6B3",
  #   "> 0.5 - 1" = "#FFD25D",
  #   "> 1 - 2" = "#FFBA00",
  #   "> 2 - 3" = "#FF9600",
  #   "> 3 - 4" = "#FF6908",
  #   "> 4 - 5" = "#E63D23",
  #   "> 5 - < 6" = "#BD251C",
  #   ">= 6" = "#8C130E"
  # )
  
  color_map = c("0 to < 0.1" = "#fff8f0", 
              "0.1 to < 0.5" = "#FFF2E1", 
                "0.5 to < 1" = "#FFEDD3", 
                "1 to < 2"   = "#FFC97A", 
                "2 to < 3"   = "#FFA521",
                "3 to < 4"   = "#FF9600",
                "4 to < 5"   = "#EB6C2A", 
                "5 to < 6"   = "#D63333", 
                "6 to < 7"   = "#8E2946", 
                ">= 7"       = "#451F59")
  
  # Render Highcharts chart
  output$chart <- renderHighchart({
    
  #  custom_colors <- c("#fff2e1", "#ffedd3","#FFD25D", "#ffa521", "#eb6c2a", "#d63333", "#8e2946", "#451f59", "#2a1333")
   # custom_colors <- c("#FFFFFF", "#FFF2E1","#FFEDD3", "#FFC97A", "#FFA521", "#EB6C2A", "#D63333", "#8E2946", "#451F59")
    custom_colors <- c("#fff8f0", "#FFF2E1","#FFEDD3", "#FFC97A", "#FFA521", "#FF9600", "#EB6C2A", "#D63333", "#8E2946", "#451F59")
    
    data_df <- clicked_data %>% head(10) %>% mutate(color = custom_colors[1:n()])
    
    
    tooltip_format <- if (input$switch_btn == "llpp") {
      "<b>{point.category}</b><br>LE Loss: {point.y} years"
    } else {
      "<b>{point.category}</b><br>PM2.5: {point.y} μg/m³"
    }
    
    axis_y <- if(input$switch_btn == "llpp"){
      list(text = "Life Expectancy Loss (Years)")
    } else {
      
      list(text = "PM2.5 Concentration (μg/m³)")
    }
    
    # Highchart Column Chart
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Top 10 Most Polluted District(s)") %>%
      hc_xAxis(categories = data_df$name2, title = list(text = "District(s)")) %>%
      hc_yAxis(title = axis_y) %>%
      hc_add_series(
        data = data_df$Value,
        name = "PM2.5",
        colorByPoint = TRUE,
        colors = data_df$color
      ) %>%
      hc_tooltip(pointFormat = tooltip_format) %>%
      
      hc_tooltip(
        pointFormat = tooltip_format
      ) %>%
      hc_plotOptions(
        column = list(
          borderRadius = 3,
          dataLabels = list(enabled = TRUE, format = "{point.y}")
        )
      ) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_legend(enabled = FALSE)
    #  hc_add_theme(hc_theme_flat())
    
  })
  
  # Render data table
  output$table <- DT::renderDataTable({
    
    req(clicked_data)  # ensures clicked_data is not NULL
    
    if (nrow(clicked_data) == 0) {
      return(NULL)
    }
    
    shiny::validate(
      need(nrow(clicked_data) > 0, "There is no data to show!")
    )
    
    
    clicked_data <- clicked_data %>% rename("Country" = name0, 
                                            "State" = name1, 
                                            "Subnationa Units" = name2, 
                                            "Population" = population 
    )
    
    DT::datatable(
      clicked_data,
      rownames = FALSE,
      options = opts1,  # ensure opts1 includes scrollY
      selection = 'single',
      extensions = 'Buttons',
      class = 'cell-border stripe compact nowrap'
    )
    
  })
  
})



output$line_pm_llppwho <- renderHighchart({
  
  if (input$switch_btn == "pm25") {
    
    if (!is.null(rv_country$country_gis) && nzchar(rv_country$country_gis)) {
      data_df <- (get_country_df())
      
      text = paste0(
        "Annual <span style='color:maroon;'>", unique(data_df$country), 
        "</span> PM<sub>2.5</sub> Concentration"
      )
      
      print("------------------------------------------------------")
      # print(data_df)

    }
    
    if (isTRUE(input$use_state) && !is.null(rv_state$level_1_id) && nzchar(rv_state$level_1_id)) {
      data_df <- (get_state_df())
      
      text = paste0(
        "Annual <span style='color:maroon;'>", unique(data_df$country), "→ ", input$level_1_id,
        "</span> PM<sub>2.5</sub> Concentration"
      )
      
      print("------------------------------------------------------")
      # print(data_df)
    }
    
    if (isTRUE(input$use_district_id) && !is.null(rv_district$level_2_id) && nzchar(rv_district$level_2_id)) {
      data_df <- (get_district_df())
      
      text = paste0(
        "Annual <span style='color:maroon;'>", unique(data_df$country), "→ ", input$level_1_id, "→ ", input$level_2_id, 
        "</span> PM<sub>2.5</sub> Concentration"
      )
    }
    
    
    # --- Highchart Code
    highchart() %>%
      hc_chart(type = "line") %>%
      
      # X-Axis: Year
      hc_xAxis(categories = data_df$year,
               title = list(text = "Year")) %>%
      
      # First Y-Axis: PM2.5
      hc_yAxis(
        title = list(text = "PM2.5 (µg/m³)")
      ) %>% 
      
    # PM2.5 series
    hc_add_series(name = "PM2.5",
                  data = data_df$pm,
                  yAxis = 0,
                  type = "spline",
                  color = "#1f77b4") %>%
      
      # PM2.5 National Standard Line
      hc_add_series(name = "PM2.5 National Standard",
                    data = data_df$natstandard,
                    yAxis = 0,
                    type = "line",
                    dashStyle = "Dash",
                    color = "grey",
                    showInLegend = TRUE) %>%
      
      # Titles
      hc_title(
        text = text,
        useHTML = TRUE
      ) %>% 
      
      hc_subtitle(text = paste0("National Avg. PM2.5 Standard ", "(", unique(data_df$natstandard), " µg/m³)")) %>%
      
      # Tooltip
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
    
  } else {
    
    print("Life Loss Expectancy--------------!")
    
    if (!is.null(rv_country$country_gis) && nzchar(rv_country$country_gis)) {
      data_df <- (get_country_df())
      text = paste0(
        "Annual Avg. <span style='color:maroon;'>", unique(data_df$country), 
        "</span> Life Expectancy Loss"
      )
    }
    
    if (isTRUE(input$use_state) && !is.null(rv_state$level_1_id) && nzchar(rv_state$level_1_id)) {
      data_df <- (get_state_df())
      text = paste0(
        "Annual Avg. <span style='color:maroon;'>", unique(data_df$country), "→ ", input$level_1_id,
        "</span> Life Expectancy Loss"
      )
    }
    
    if (isTRUE(input$use_district_id) && !is.null(rv_district$level_2_id) && nzchar(rv_district$level_2_id)) {
      data_df <- (get_district_df())
      text = paste0(
        "Annual Avg. <span style='color:maroon;'>", unique(data_df$country), "→ ", input$level_1_id, "→ ", input$level_2_id, 
        "</span> Life Expectancy Loss"
      )
    }
    

    # --- Highchart Code
    highchart() %>%
      hc_chart(type = "line") %>%
      
      # X-Axis: Year
      hc_xAxis(categories = data_df$year,
               title = list(text = "Year")) %>%
      
      # First Y-Axis: Life Expectancy Loss (Years)
      hc_yAxis(
        title = list(text = "Life Expectancy Loss (Years)")
      ) %>% 
      
      
      # Life Expectancy Loss series
      hc_add_series(name = "Life Expectancy Loss",
                    data = data_df$llpp_who,
                    yAxis = 0,
                    type = "spline",
                    color = "#ff7f0e") %>%
      
      # PM2.5 National Standard Line
      # hc_add_series(name = "PM2.5 National Standard",
      #               data = data_df$natstandard,
      #               yAxis = 0,
      #               type = "line",
      #               dashStyle = "Dash",
      #               color = "grey",
      #               showInLegend = TRUE) %>%
      
      # Titles
    hc_title(
      text = text,
      useHTML = TRUE
    ) %>% 
      
      hc_subtitle(text = paste0("National PM2.5 Standard ", "(", unique(data_df$natstandard), " µg/m³)")) %>%
      
      # Tooltip
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
    
    
    
  }
  
  
  
})

observe({
  print("input$global_level_choice1")
  
  print(input$global_level_choice1)
})
#################
output$top_10_polluted_populated <- renderHighchart({
  
  req(
    input$global_level_choice1,
    input$country_gis,
    input$year_gis
  )
  
  custom_colors <- c(
    "#fff8f0", "#FFF2E1", "#FFEDD3", "#FFC97A",
    "#FFA521", "#FF9600", "#EB6C2A",
    "#D63333", "#8E2946", "#451F59"
  )
  
  # =========================
  # DISTRICT LEVEL
  # =========================
 if (input$global_level_choice1 == "district_choice1") {
    
    data_df <- gadm2_pm25 %>%
      filter(name0 == input$country_gis) %>%
      select(
        name0,
        name1,
        name2,
        all_of(input$year_gis),
        population
      ) %>%
      rename(Value = !!sym(input$year_gis)) %>%
      arrange(Value) %>%
      tail(10) %>%
      mutate(color = custom_colors[1:n()]) %>%
      arrange(desc(Value))
    
    data_points <- purrr::pmap(
      list(
        value   = data_df$Value,
        country = data_df$name0,
        state   = data_df$name1,
        district= data_df$name2,
        color   = data_df$color
      ),
      function(value, country, state, district, color) {
        list(
          name  = district,
          y     = value,
          color = color,
          custom = list(
            country = country,
            state   = state,
            district= district
          )
        )
      }
    )
    
   return(
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_title(text = "Top 10 Most Polluted District(s)") %>%
        hc_xAxis(
          categories = data_df$name2,
          title = list(text = "District"),
          reversed = TRUE
        ) %>%
        hc_yAxis(
          title = list(text = "PM2.5 Concentration (μg/m³)")
        ) %>%
        hc_add_series(
          name = "PM2.5",
          data = data_points,
          colorByPoint = TRUE
        ) %>%
        hc_tooltip(
          useHTML = TRUE,
          formatter = JS(
            "function () {
              return '<b>' + this.point.name + '</b><br>' +
                     'PM2.5: ' + this.point.y.toFixed(1) + ' μg/m³<br>' +
                     'State: ' + this.point.custom.state;
            }"
          )
        ) %>%
        hc_plotOptions(
          bar = list(
            borderRadius = 4,
            dataLabels = list(enabled = TRUE, format = "{point.y:.1f}")
          )
        ) %>%
        hc_legend(enabled = FALSE)
  )
  }
  
  # =========================
  # STATE LEVEL
  # =========================
  if (input$global_level_choice1 == "state_choice1") {

    data_df <- gadm1_pm25 %>%
      filter(name0 == input$country_gis) %>%
      select(
        name0,
        name1,
        all_of(input$year_gis),
        population
      ) %>%
      rename(Value = !!sym(input$year_gis)) %>%
      arrange(Value) %>%
      tail(10) %>%
      mutate(color = custom_colors[1:n()]) %>%
      arrange(desc(Value))

    data_points <- purrr::pmap(
      list(
        value   = data_df$Value,
        country = data_df$name0,
        state   = data_df$name1,
        color   = data_df$color
      ),
      function(value, country, state, color) {
        list(
          name  = state,
          y     = value,
          color = color,
          custom = list(country = country)
        )
      }
    )

    return(
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_title(text = "Top 10 Most Polluted State(s)") %>%
        hc_xAxis(
          categories = data_df$name1,
          title = list(text = "State"),
          reversed = TRUE
        ) %>%
        hc_yAxis(
          title = list(text = "PM2.5 Concentration (μg/m³)")
        ) %>%
        hc_add_series(
          name = "PM2.5",
          data = data_points,
          colorByPoint = TRUE
        ) %>%
        hc_legend(enabled = FALSE)
    )
  }
  
})


output$top_10_polluted_populated_who <- renderHighchart({
  
  # Define bins for Life Expectancy Loss (0–10 years, WHO)
  bins <- c(-Inf, 1, 2, 3, 4, 5, 6, 7, 8, 9, Inf)
  
  # Labels for bins
  bin_labels <- c(
    "0 - <1", "1 - 2", ">2 - 3", ">3 - 4", ">4 - 5",
    ">5 - 6", ">6 - 7", ">7 - 8", ">8 - 9", ">=9"
  )
  
  # Orange-based color map (lighter → deeper orange, no dark purples)
  color_map <- c(
    "0 - <1"   = "#fff2e1",  # very light cream
    "1 - 2"    = "#ffedd3",  
    ">2 - 3"   = "#FFD25D",  
    ">3 - 4"   = "#ffc97a",  
    ">4 - 5"   = "#ffa521",  
    ">5 - 6"   = "#eb6c2a",  
    ">6 - 7"   = "#ff4d00",  # vivid orange
    ">7 - 8"   = "#e63f00",  # strong burnt orange
    ">8 - 9"   = "#cc3300",  # deep orange
    ">=9"      = "#b32b00"   # darkest orange (but not maroon)
  )
  
  # Prepare data for Highchart
  if (input$global_level_choice1 == "district_choice1") {
    
  data_df <- gadm2_llppwho %>%
    filter(name0 == input$country_gis) %>%
    select(name0, name1, name2, !!input$year_gis, population) %>%
    rename(Value = !!sym(input$year_gis)) %>%
    arrange(Value) %>%
    tail(10) %>%
    arrange(desc(Value)) %>%
    mutate(
      bin = cut(Value, breaks = bins, labels = bin_labels, right = FALSE),
      color = color_map[as.character(bin)]
    )
  
  
  # Create list of data points for Highchart
  data_points <- purrr::pmap(
    list(data_df$name2, data_df$Value, data_df$name0, data_df$name1, data_df$name2, data_df$color),
    function(name2, value, country, state, subnat, color) {
      list(
        name = name2,
        y = value,
        color = color,
        custom = list(
          country = country,
          state = state,
          subnational = subnat
        )
      )
    }
  )
  
  
  # Highchart Column Chart
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_title(text = "Top 10 District(s) by Life Expectancy Loss") %>%
    hc_xAxis(categories = data_df$name2, title = list(text = "District(s)")) %>%
    hc_yAxis(title = list(text = "Potential Gain in Life Expectancy (Years)")) %>%
    hc_add_series(
      data = data_points,
      name = "PM2.5",
      colorByPoint = TRUE
    ) %>%
    hc_tooltip(
      useHTML = TRUE,
      formatter = JS(
        "function() {
         return '<b>' + this.point.name + '</b><br>' +
                'PM2.5: ' + this.point.y + ' Years<br>' +
                'Country: ' + this.point.custom.country + '<br>' +
                'State/Province: ' + this.point.custom.state + '<br>' +
                'Subnational: ' + this.point.custom.subnational;
       }"
      )
    ) %>%
    hc_plotOptions(
      bar = list(
        borderRadius = 3,
        dataLabels = list(enabled = TRUE, format = "{point.y}")
      )
    ) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = FALSE)
  
  }else{
    
    data_df <- gadm1_llppwho %>%
      filter(name0 == input$country_gis) %>%
      select(name0, name1, all_of(input$year_gis), population) %>%
      rename(Value = !!sym(input$year_gis)) %>%
      arrange(Value) %>%
      tail(10) %>%
      arrange(desc(Value)) %>%
      mutate(
        bin   = cut(Value, breaks = bins, labels = bin_labels, right = FALSE),
        color = color_map[as.character(bin)]
      )
    
    data_points <- purrr::pmap(
      list(
        state   = data_df$name1,
        value   = data_df$Value,
        country = data_df$name0,
        color   = data_df$color
      ),
      function(state, value, country, color) {
        list(
          name  = state,
          y     = value,
          color = color,
          custom = list(country = country)
        )
      }
    )
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Top 10 State(s) by Life Expectancy Loss") %>%
      hc_xAxis(
        categories = data_df$name1,
        title = list(text = "State"),
        reversed = TRUE
      ) %>%
      hc_yAxis(
        title = list(text = "Potential Gain in Life Expectancy (Years)")
      ) %>%
      hc_add_series(
        name = "Life Expectancy Loss",
        data = data_points,
        colorByPoint = TRUE
      ) %>%
      hc_legend(enabled = FALSE)
    
    
  }
  
})


output$top_10_polluted_populated_nat <- renderHighchart({
  
  # Define bins for Life Expectancy Loss (0–10 years, WHO)
  bins <- c(-Inf, 1, 2, 3, 4, 5, 6, 7, 8, 9, Inf)
  
  # Labels for bins
  bin_labels <- c(
    "0 - <1", "1 - 2", ">2 - 3", ">3 - 4", ">4 - 5",
    ">5 - 6", ">6 - 7", ">7 - 8", ">8 - 9", ">=9"
  )
  
  # Orange-based color map (lighter → deeper orange, no dark purples)
  color_map <- c(
    "0 - <1"   = "#fff2e1",  # very light cream
    "1 - 2"    = "#ffedd3",  
    ">2 - 3"   = "#FFD25D",  
    ">3 - 4"   = "#ffc97a",  
    ">4 - 5"   = "#ffa521",  
    ">5 - 6"   = "#eb6c2a",  
    ">6 - 7"   = "#ff4d00",  # vivid orange
    ">7 - 8"   = "#e63f00",  # strong burnt orange
    ">8 - 9"   = "#cc3300",  # deep orange
    ">=9"      = "#b32b00"   # darkest orange (but not maroon)
  )
  
  if (input$global_level_choice1 == "district_choice1") {
    
  # Prepare data for Highchart
  data_df <- gadm2_llpp_nat %>%
    filter(name0 == input$country_gis) %>%
    select(name0, name1, name2, !!input$year_gis, population) %>%
    rename(Value = !!sym(input$year_gis)) %>%
    arrange(Value) %>%
    tail(10) %>%
    arrange(desc(Value)) %>%
    mutate(
      bin = cut(Value, breaks = bins, labels = bin_labels, right = FALSE),
      color = color_map[as.character(bin)]
    )

  
  

  # Create list of data points for Highchart
  data_points <- purrr::pmap(
    list(data_df$name2, data_df$Value, data_df$name0, data_df$name1, data_df$name2, data_df$color),
    function(name2, value, country, state, subnat, color) {
      list(
        name = name2,
        y = value,
        color = color,
        custom = list(
          country = country,
          state = state,
          subnational = subnat
        )
      )
    }
  )
  
  
  # Highchart Column Chart
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_title(text = "Top 10 District(s) by Life Expectancy Loss") %>%
    hc_xAxis(categories = data_df$name2, title = list(text = "District(s)")) %>%
    hc_yAxis(title = list(text = "Life Expectancy Loss (Years)")) %>%
    hc_add_series(
      data = data_points,
      name = "PM2.5",
      colorByPoint = TRUE
    ) %>%
    hc_tooltip(
      useHTML = TRUE,
      formatter = JS(
        "function() {
         return '<b>' + this.point.name + '</b><br>' +
                'PM2.5: ' + this.point.y + ' Years<br>' +
                'Country: ' + this.point.custom.country + '<br>' +
                'State/Province: ' + this.point.custom.state + '<br>' +
                'Subnational: ' + this.point.custom.subnational;
       }"
      )
    ) %>%
    hc_plotOptions(
      bar = list(
        borderRadius = 3,
        dataLabels = list(enabled = TRUE, format = "{point.y}")
      )
    ) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = FALSE)
  }else{
    
    # Prepare data for Highchart
    data_df <- gadm1_llpp_nat %>%
      filter(name0 == input$country_gis) %>%
      select(name0, name1, !!input$year_gis, population) %>%
      rename(Value = !!sym(input$year_gis)) %>%
      arrange(Value) %>%
      tail(10) %>%
      arrange(desc(Value)) %>%
      mutate(
        bin = cut(Value, breaks = bins, labels = bin_labels, right = FALSE),
        color = color_map[as.character(bin)]
      )
    
    
    
    
    # Create list of data points for Highchart

    data_points <- purrr::pmap(
      list(
        state   = data_df$name1,
        value   = data_df$Value,
        country = data_df$name0,
        color   = data_df$color
      ),
      function(state, value, country, color) {
        list(
          name  = state,
          y     = value,
          color = color,
          custom = list(country = country)
        )
      }
    )
    
    
    # Highchart Column Chart
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Top 10 State(s) by Life Expectancy Loss") %>%
      hc_xAxis(categories = data_df$name1, title = list(text = "District(s)")) %>%
      hc_yAxis(title = list(text = "Life Expectancy Loss (Years)")) %>%
      hc_add_series(
        data = data_points,
        name = "PM2.5",
        colorByPoint = TRUE
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(
          "function() {
         return '<b>' + this.point.name + '</b><br>' +
                'PM2.5: ' + this.point.y + ' Years<br>' +
                'Country: ' + this.point.custom.country + '<br>' +
                'State/Province: ' + this.point.custom.state + '<br>' 
       }"
        )
      ) %>%
      hc_plotOptions(
        bar = list(
          borderRadius = 3,
          dataLabels = list(enabled = TRUE, format = "{point.y}")
        )
      ) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_legend(enabled = FALSE)
    
    
  }
  
})


output$top_10_populated <- renderHighchart({
  
  

  

    
    
    # data_df <- gadm2_pm25[
    #   name0 == input$country_gis, 
    #   .(name0, name1, name2, population, Value = get(input$year_gis))
    # ][order(population)][.N-9:.N][order(-population)][, pop := round(population / 1e5)]  # new column like mutate()
    # 
    # 
  
  # Define bins for population in lakhs (up to 50+)
 
  
  # Extend your color palette to match bins (same gradient idea)
  color_map <- c(
    "0 - < 1"   = "#FFFFFF",
    "1 - 3"  = "#FFE6B3",
    ">3 - 6"   = "#FFD25D",
    ">6 - 9"     = "#FFBA00",
    ">9 - 12"     = "#FF9600",
    ">12 - 15"    = "#FF6908",
    ">15 - 20"  = "#FF8533",  # softer orange
    ">20 - 25"  = "#FF751A",  # medium orange
    ">25 - 30"  = "#FF5C00",  # vivid orange
    ">35 - 40"  = "#E64A00",  # deep burnt orange
    ">= 40"     = "#CC3D00"   # darker burnt orange
  )
  
  if (input$global_level_choice1 == "district_choice1") {
    
  # Add bin + color into your data
  data_df <- gadm2_pm25 %>%
    filter(name0 == input$country_gis) %>%
    select(name0, name1, name2, !!input$year_gis, population) %>%
    arrange(population) %>%
    tail(10) %>%
    arrange(desc(population)) %>%
    mutate(
      pop = round(population / 100000, 0))  # Convert to lakhs
  
  pop_bins <- cut(
    data_df$pop,
    breaks = c(-Inf, 1, 3, 6, 9, 12, 15, 20, 25, 30, 40, Inf),
    labels = c(
      "0 - < 1", "1 - 3", ">3 - 6", ">6 - 9", ">9 - 12", 
      ">12 - 15", ">15 - 20", ">20 - 25", ">25 - 30", 
      ">35 - 40", ">= 40"
    ),
    right = FALSE
  )
  
  data_df$bin <- pop_bins
  data_df$color <- color_map[as.character(data_df$bin)]
  
  # Prepare data points (same as your code, but now uses new colors)
  data_points <- purrr::pmap(
    list(data_df$name2, data_df$pop, data_df$name0, data_df$name1, data_df$name2, data_df$color),
    function(name2, value, country, state, subnat, color) {
      list(
        name = name2,
        y = value,  
        color = color,
        custom = list(
          country = country,
          state = state,
          subnational = subnat
        )
      )
    }
  )
  
  # Highchart with fixed color scale
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_title(text = "Top 10 Most Populated District(s)") %>%
    hc_xAxis(categories = data_df$name2, title = list(text = "District(s)")) %>%
    hc_yAxis(title = list(text = "Population (in Lakhs)")) %>%
    hc_add_series(
      data = data_points,
      name = "Population",
      colorByPoint = TRUE
    ) %>%
    hc_tooltip(
      useHTML = TRUE,
      formatter = JS(
        "function() {
       return '<b>' + this.point.name + '</b><br>' +
              'Population: ' + this.point.y + ' Lakhs<br>' +
              'Country: ' + this.point.custom.country + '<br>' +
              'State/Province: ' + this.point.custom.state + '<br>' +
              'Subnational: ' + this.point.custom.subnational;
     }"
      )
    ) %>%
    hc_plotOptions(
      bar = list(
        borderRadius = 3,
        dataLabels = list(enabled = TRUE, format = "{point.y}")
      )
    ) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = FALSE)
  
  } else{
    
    data_df <- gadm1_pm25 %>%
      filter(name0 == input$country_gis) %>%
      select(name0, name1, population) %>%
      arrange(population) %>%
      tail(10) %>%
      arrange(desc(population)) %>%
      mutate(
        pop = round(population / 100000, 0)  # population in lakhs
      )
    
    data_df$bin <- cut(
      data_df$pop,
      breaks = c(-Inf, 1, 3, 6, 9, 12, 15, 20, 25, 30, 40, Inf),
      labels = c(
        "0 - < 1", "1 - 3", ">3 - 6", ">6 - 9", ">9 - 12",
        ">12 - 15", ">15 - 20", ">20 - 25", ">25 - 30",
        ">35 - 40", ">= 40"
      ),
      right = FALSE
    )
    
    data_df$color <- color_map[as.character(data_df$bin)]
    
    data_points <- purrr::pmap(
      list(
        state   = data_df$name1,
        value   = data_df$pop,
        country = data_df$name0,
        color   = data_df$color
      ),
      function(state, value, country, color) {
        list(
          name  = state,
          y     = value,
          color = color,
          custom = list(
            country = country,
            state   = state
          )
        )
      }
    )
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Top 10 Most Populated States") %>%
      hc_xAxis(
        categories = data_df$name1,
        title = list(text = "State"),
        reversed = TRUE
      ) %>%
      hc_yAxis(
        title = list(text = "Population (in Lakhs)")
      ) %>%
      hc_add_series(
        name = "Population",
        data = data_points,
        colorByPoint = TRUE
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(
          "function () {
        return '<b>' + this.point.name + '</b><br>' +
               'Population: <b>' + this.point.y + ' lakhs</b><br>' +
               'Country: ' + this.point.custom.country;
      }"
        )
      ) %>%
      hc_plotOptions(
        bar = list(
          borderRadius = 3,
          dataLabels = list(enabled = TRUE, format = '{point.y}')
        )
      ) %>%
      hc_legend(enabled = FALSE)
    
  
  }
  
})

