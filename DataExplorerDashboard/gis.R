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
        )
      
      
    }
    
    ########## click events
   
    
    
  })


# On click, show modal with chart + table
# Debug: Monitor click events
observe({
  print("Checking map_shape_click:")
  print(input)
})

observe({
  print("Shape click object:")
  str(input$country_wise_pm_llp_shape_click$id)
})


# Handle polygon click
observeEvent(input$country_wise_pm_llp_shape_click, {
  print("Map shape clicked!")
  click_id <- input$country_wise_pm_llp_shape_click$id
  if (is.null(click_id)) {
    print("No click ID detected")
    return()
  }
  
  print(paste("Clicked state:", click_id))
  state_id <- click_id
  if (input$switch_btn == "pm25") {
    
    
    clicked_data <- gadm2_shp_pm25 %>% filter(name0 == input$country_gis) %>%
      select(name0, name1,name2,population, !!input$year_gis) %>% filter() %>% filter(name1 == state_id) %>% 
      rename(Value = !!sym(input$year_gis)) %>% arrange(Value)
    

  }
    else{

      clicked_data <- gadm2_shp_llppwho %>% filter(name0 == input$country_gis) %>%
        select(name0, name1,name2,population, !!input$year_gis) %>% filter() %>% filter(name1 == state_id) %>% 
        rename(Value = !!sym(input$year_gis)) %>% arrange(Value)
      
    }
    
  
 
   print(clicked_data)
  
  
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
  
  
  # Render Highcharts chart
  output$chart <- renderHighchart({
    
    custom_colors <- c("#fff2e1", "#ffedd3","yellow", "#ffc97a", "#ffa521", "#eb6c2a", "#d63333", "#8e2946", "#451f59", "#2a1333")
    
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
      hc_title(text = "Top 10 Most Polluted Subnational Units") %>%
      hc_xAxis(categories = data_df$name2, title = list(text = "Subnational Unit")) %>%
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
    
  data_df <- gadm0_long %>%
    filter(country == input$country_gis)

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
    # hc_yAxis_multiples(
    #   list( # yAxis[0] for PM2.5
    #     title = list(text = "PM2.5 (µg/m³)"),
    #     opposite = FALSE
    #   ),
    #   list( # yAxis[1] for Life Expectancy Loss
    #     title = list(text = "Life Expectancy Loss (Years)"),
    #     opposite = TRUE
    #   )
    # ) %>%

    # PM2.5 series
    hc_add_series(name = "PM2.5",
                  data = data_df$pm,
                  yAxis = 0,
                  type = "spline",
                  color = "#1f77b4") %>%

    # Life Expectancy Loss series
    # hc_add_series(name = "Life Expectancy Loss",
    #               data = data_df$llpp_who,
    #               yAxis = 1,
    #               type = "spline",
    #               color = "#ff7f0e") %>%

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
      text = paste0(
        "Annual <span style='color:maroon;'>", unique(data_df$country), 
        "</span> PM<sub>2.5</sub> Concentration"
      ),
      useHTML = TRUE
    ) %>% 
  
    hc_subtitle(text = paste0("National Avg. PM2.5 Standard ", "(", unique(data_df$natstandard), " µg/m³)")) %>%

    # Tooltip
    hc_tooltip(shared = TRUE, crosshairs = TRUE)
  
  } else {
    
    print("Life Loss Expectancy--------------!")
    
    data_df <- gadm0_long %>%
      filter(country == input$country_gis)
    
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
        text = paste0(
          "Annual Avg. <span style='color:maroon;'>", unique(data_df$country), 
          "</span> Life Expectancy Loss"
        ),
        useHTML = TRUE
      ) %>% 
      
      hc_subtitle(text = paste0("National PM2.5 Standard ", "(", unique(data_df$natstandard), " µg/m³)")) %>%
      
      # Tooltip
      hc_tooltip(shared = TRUE, crosshairs = TRUE)
    
    
    
  }



})


##################
output$top_10_polluted_populated <- renderHighchart({
  
custom_colors <- c("#fff2e1", "#ffedd3","yellow", "#ffc97a", "#ffa521", "#eb6c2a", "#d63333", "#8e2946", "#451f59", "#2a1333")
  
# Prepare data for Highchart
data_df <- gadm2_shp_pm25 %>%
  filter(name0 == input$country_gis) %>%
  select(name0, name1, name2, !!input$year_gis, population) %>%
  rename(Value = !!sym(input$year_gis)) %>%
  arrange((Value)) %>%
  tail(10) %>%
  mutate(color = custom_colors[1:n()]) %>% arrange(desc(Value))

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
  hc_title(text = "Top 10 Most Polluted Subnational Units") %>%
  hc_xAxis(categories = data_df$name2, title = list(text = "Subnational Unit")) %>%
  hc_yAxis(title = list(text = "PM2.5 Concentration (μg/m³)")) %>%
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
                'PM2.5: ' + this.point.y + ' μg/m³<br>' +
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

})


output$top_10_polluted_populated_who <- renderHighchart({
  
  custom_colors <- c("#fff2e1", "#ffedd3","yellow", "#ffc97a", "#ffa521", "#eb6c2a", "#d63333", "#8e2946", "#451f59", "#2a1333")
  
  # Prepare data for Highchart
  data_df <- gadm2_shp_llppwho %>%
    filter(name0 == input$country_gis) %>%
    select(name0, name1, name2, !!input$year_gis, population) %>%
    rename(Value = !!sym(input$year_gis)) %>%
    arrange((Value)) %>%
    tail(10) %>%
    mutate(color = custom_colors[1:n()]) %>% arrange(desc(Value))
  
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
    hc_title(text = "Top 10 Subnational Units by Life Expectancy Loss") %>%
    hc_xAxis(categories = data_df$name2, title = list(text = "Subnational Unit")) %>%
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
  
})


output$top_10_polluted_populated_nat <- renderHighchart({
  
  custom_colors <- c("#fff2e1", "#ffedd3","yellow", "#ffc97a", "#ffa521", "#eb6c2a", "#d63333", "#8e2946", "#451f59", "#2a1333")
  
  # Prepare data for Highchart
  data_df <- gadm2_shp_llpp_nat %>%
    filter(name0 == input$country_gis) %>%
    select(name0, name1, name2, !!input$year_gis, population) %>%
    rename(Value = !!sym(input$year_gis)) %>%
    arrange((Value)) %>%
    tail(10) %>%
    mutate(color = custom_colors[1:n()]) %>% arrange(desc(Value))
  
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
    hc_title(text = "Top 10 Subnational Units by Life Expectancy Loss") %>%
    hc_xAxis(categories = data_df$name2, title = list(text = "Subnational Unit")) %>%
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
  
})


output$top_10_populated <- renderHighchart({
  
  custom_colors <- c("#fff2e1", "#ffedd3", "yellow", "#ffc97a", "#ffa521",
                     "#eb6c2a", "#d63333", "#8e2946", "#451f59", "#2a1333")
  
  # Prepare data for Highchart
  data_df <- gadm2_shp_pm25 %>%
    filter(name0 == input$country_gis) %>%
    select(name0, name1, name2, !!input$year_gis, population) %>%
    arrange(population) %>%
    tail(10) %>%
    mutate(color = custom_colors[1:n()]) %>%
    arrange(desc(population)) %>%
    mutate(pop = round(population / 100000, 0))  # Convert to lakhs
  
  # Create list of data points for Highchart
  data_points <- purrr::pmap(
    list(data_df$name2, data_df$pop, data_df$name0, data_df$name1, data_df$name2, data_df$color),
    function(name2, value, country, state, subnat, color) {
      list(
        name = name2,
        y = value,  # ✅ Corrected from `pop` to `value`
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
    hc_title(text = "Top 10 Most Populated Subnational Units") %>%
    hc_xAxis(categories = data_df$name2, title = list(text = "Subnational Unit")) %>%
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
  
})

# output$top_10_polluted_populated <- renderHighchart({
#   
#   custom_colors <- c("#fff2e1", "#ffedd3","yellow", "#ffc97a", "#ffa521", "#eb6c2a", "#d63333", "#8e2946", "#451f59", "#2a1333")
#   
#   data_df <- gadm2_shp_pm25 %>%
#     filter(name0 == input$country_gis) %>%
#     select(name0, name1, name2, !!input$year_gis, population)  %>% rename(Value = !!sym(input$year_gis)) %>% arrange(Value) %>% head(10) %>% mutate(color = custom_colors[1:n()])
#   
#   
#   # Highchart Column Chart
#   highchart() %>%
#     hc_chart(type = "bar") %>%
#     hc_title(text = "Top 10 Most Polluted Subnational Units") %>%
#     hc_xAxis(categories = data_df$name2, title = list(text = "Subnational Unit")) %>%
#     hc_yAxis(title = list(text = "PM2.5 Concentration (μg/m³)")) %>%
#     hc_add_series(
#       data = data_df$Value,
#       name = "PM2.5",
#       colorByPoint = TRUE,
#       colors = data_df$color
#     ) %>%
#     hc_tooltip(
#       pointFormat = "<b>{point.category}</b><br>PM2.5: {point.y} μg/m³"
#     ) %>%
#     hc_plotOptions(
#       column = list(
#         borderRadius = 3,
#         dataLabels = list(enabled = TRUE, format = "{point.y}")
#       )
#     ) %>%
#     hc_exporting(enabled = TRUE) %>% 
#     hc_legend(enabled = FALSE)
# 
# })



# output$top_10_polluted_populated <- renderHighchart({
#   
# 
#   # --- Highchart Code
#   highchart() %>%
#     hc_chart(type = "Column") %>%
#     
#     # X-Axis: Year
#     hc_xAxis(categories = data_df$year,
#              title = list(text = "Year")) %>%
#     
#     # First Y-Axis: PM2.5
#     hc_yAxis_multiples(
#       list( # yAxis[0] for PM2.5
#         title = list(text = "PM2.5 (µg/m³)"),
#         opposite = FALSE
#       ),
#       list( # yAxis[1] for Life Expectancy Loss
#         title = list(text = "Life Expectancy Loss (Years)"),
#         opposite = TRUE
#       )
#     ) %>%
#     
#     # PM2.5 series
#     hc_add_series(name = "PM2.5",
#                   data = data_df$pm,
#                   yAxis = 0,
#                   type = "spline",
#                   color = "#1f77b4") %>%
#     
#     # Life Expectancy Loss series
#     hc_add_series(name = "Life Expectancy Loss",
#                   data = data_df$llpp_who,
#                   yAxis = 1,
#                   type = "spline",
#                   color = "#ff7f0e") %>%
#     
#     # PM2.5 National Standard Line
#     hc_add_series(name = "PM2.5 National Standard",
#                   data = data_df$natstandard,
#                   yAxis = 0,
#                   type = "line",
#                   dashStyle = "Dash",
#                   color = "grey",
#                   showInLegend = TRUE) %>%
#     
#     # Titles
#     hc_title(
#       text = paste0(
#         "Year-wise <span style='color:maroon;'>", unique(data_df$country), "</span> PM<sub>2.5</sub> and Life Expectancy Loss"
#       ),
#       useHTML = TRUE
#     ) %>% 
#     
#     hc_subtitle(text = paste0("With National PM2.5 Standard Line ", "(", unique(data_df$natstandard), " µg/m³)")) %>%
#     
#     # Tooltip
#     hc_tooltip(shared = TRUE, crosshairs = TRUE)
#   
#   
#   
# })
