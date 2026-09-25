# ============================================================
# Author:        Purushottam Gupta
# Organization:  Air Quality Life Index (AQLI)
#                University of Chicago
# Email:         guptap@uchicago.edu
#
# Description:
# Shiny reactive filtering system with cascading filters
# (Continent → Region → Country → State → District)
# ============================================================

# ---------------------------------------------------------
# Reactive values to store current selections for each level
# ---------------------------------------------------------

rv_continent_aq  <- reactiveValues()   # stores selected continent
rv_aqregion_aq   <- reactiveValues()   # stores selected AQLI region
rv_country_aq    <- reactiveValues()   # stores selected country
rv_state_aq      <- reactiveValues()   # stores selected state/province
rv_district_aq   <- reactiveValues()   # stores selected district


# ---------------------------------------------------------
# Initialize reactive values from input selections
# isolate() prevents unnecessary reactivity at initialization
# ---------------------------------------------------------

isolate({
  
  rv_continent_aq$continent_aq <- input$continent_aq
  rv_aqregion_aq$region_aq     <- input$region_aq
  rv_country_aq$country_aq     <- input$country_aq
  rv_state_aq$state_aq         <- input$state_aq
  rv_district_aq$district_aq   <- input$district_aq
  
  # rv_year_bs$year <- input$year   # optional year selection
})



# ---------------------------------------------------------
# Update selection inputs when geographic level changes
# (Continent → Region → Country → State → District)
# ---------------------------------------------------------

observeEvent(input$level_choice, {
  
  # Store current selections to preserve them if possible
  current_continent <- input$continent_aq
  current_country   <- input$country_aq
  current_region    <- input$region_aq
  current_state     <- input$state_aq
  current_district  <- input$district_aq
  
  
  # -----------------------------------------------------
  # Update Continent choices
  # -----------------------------------------------------
  
  continent_choices <- sort(unique(select_filter$continent))
  
  # Keep previous selection if still valid
  continent_selected <- intersect(current_continent, continent_choices)
  
  if (length(continent_selected) == 0)
    continent_selected <- unique(select_filter$continent)
  
  updatePickerInput(session, "continent_aq",
                    choices = continent_choices,
                    selected = continent_selected)
  
  
  # -----------------------------------------------------
  # Update AQLI Region choices based on selected continent
  # -----------------------------------------------------
  
  region_choices <- select_filter[
    continent %in% continent_selected, unique(region)
  ]
  
  region_selected <- intersect(current_region, region_choices)
  
  # Default region if none selected
  if (length(region_selected) == 0 &&
      input$level_choice %in% c("AQLI Region", "Country","State","District"))
    region_selected <- "Asia"
  
  updatePickerInput(session, "region_aq",
                    choices = c(region_choices),
                    selected = region_selected)
  
  
  # -----------------------------------------------------
  # Update Country choices based on continent + region
  # -----------------------------------------------------
  
  country_choices <- select_filter[
    continent %in% continent_selected &
      region %in% region_selected,
    unique(country)
  ]
  
  country_selected <- intersect(current_country, country_choices)
  
  # Default country selection
  if (length(country_selected) == 0 &&
      input$level_choice %in% c("Country", "State", "District"))
    country_selected <- "India"
  
  updatePickerInput(session, "country_aq",
                    choices = c(country_choices),
                    selected = country_selected)
  
  
  # -----------------------------------------------------
  # Update State/Province choices
  # -----------------------------------------------------
  
  state_choices <- select_filter[
    continent %in% continent_selected &
      region %in% region_selected &
      country %in% country_selected,
    unique(name_1)
  ]
  
  state_selected <- intersect(current_state, state_choices)
  
  # Default state selection
  if (length(state_selected) == 0 &&
      input$level_choice %in% c("State", "District"))
    state_selected <- "NCT of Delhi"
  
  updatePickerInput(session, "state_aq",
                    choices = c(state_choices),
                    selected = state_selected)
  
  
  # -----------------------------------------------------
  # Update District choices
  # -----------------------------------------------------
  
  district_choices <- select_filter[
    continent %in% continent_selected &
      region %in% region_selected &
      country %in% country_selected &
      name_1 %in% state_selected,
    unique(name_2)
  ]
  
  district_selected <- intersect(current_district, district_choices)
  
  # Default district selection
  if (length(district_selected) == 0 &&
      input$level_choice == "District")
    district_selected <- state_selected
  
  updatePickerInput(session, "district_aq",
                    choices = c(district_choices),
                    selected = district_selected)
})



# ---------------------------------------------------------
# Observe selection updates and store them in reactiveValues
# This ensures selections are only updated when dropdowns
# are not actively open by the user
# ---------------------------------------------------------

observe({
  
  # print("Observer triggered for geographic selections")
  
  if(!isTRUE(input$continent_aq_open) &
     !isTRUE(input$region_aq_open) &
     !isTRUE(input$country_aq_open) &
     !isTRUE(input$state_aq_open) &
     !isTRUE(input$district_aq_open))
  {
    
    rv_continent_aq$continent_aq <- input$continent_aq
    rv_aqregion_aq$region_aq     <- input$region_aq
    rv_country_aq$country_aq     <- input$country_aq
    rv_state_aq$state_aq         <- input$state_aq
    rv_district_aq$district_aq   <- input$district_aq
    
    # rv_year_bs$year <- input$year
  }
})

JS_Zoom <- JS(
  "function selectPointsByDrag(e) {

                 // Select points
                 Highcharts.each(this.series, function (series) {
                   Highcharts.each(series.points, function (point) {
                     if (point.x >= e.xAxis[0].min && point.x <= e.xAxis[0].max &&
                         point.y >= e.yAxis[0].min && point.y <= e.yAxis[0].max) {
                       point.select(true, true);
                     }
                   });
                 });

                 // Fire a custom event
                 Highcharts.fireEvent(this, 'selectedpoints', { points: this.getSelectedPoints() });

                 return false; // Don't zoom
               }"
)  

# ---------------------------------------------------------
# Final filtered datasets based on geographic selections
# and population range filter
# ---------------------------------------------------------

# District-level filtered dataset (GADM2)
gadm_df_aqli_district <- reactive({
  gadm2_aqli[
    (continent %in% rv_continent_aq$continent_aq) &
      (region    %in% rv_aqregion_aq$region_aq) &
      (country   %in% rv_country_aq$country_aq) &
      (name_1    %in% rv_state_aq$state_aq) &
      (name_2    %in% rv_district_aq$district_aq) &
      (population >= input$pop_range[1]) &
      (population <= input$pop_range[2])
  ]
})


# ---------------------------------------------------------
# State / Province-level filtered dataset (GADM1)
# ---------------------------------------------------------

gadm_df_aqli_state <- reactive({
  gadm1_aqli[
    (continent %in% rv_continent_aq$continent_aq) &
      (region    %in% rv_aqregion_aq$region_aq) &
      (country   %in% rv_country_aq$country_aq) &
      (name_1    %in% rv_state_aq$state_aq) &
      (population >= input$pop_range[1]) &
      (population <= input$pop_range[2])
  ]
})


# ---------------------------------------------------------
# Country-level filtered dataset (GADM0)
# ---------------------------------------------------------

gadm_df_aqli_country <- reactive({
  gadm0_aqli[
    (continent %in% rv_continent_aq$continent_aq) &
      (region    %in% rv_aqregion_aq$region_aq) &
      (country   %in% rv_country_aq$country_aq) &
      (population >= input$pop_range[1]) &
      (population <= input$pop_range[2])
  ]
})


# ---------------------------------------------------------
# AQLI Region-level dataset (population-weighted aggregates)
# ---------------------------------------------------------

gadm_df_aqli_region <- reactive({
  aqli_region_weighted[
    #(continent %in% rv_continent_aq$continent_aq) &  # optional filter
    (region %in% rv_aqregion_aq$region_aq) &
      (population >= input$pop_range[1]) &
      (population <= input$pop_range[2])
  ]
})


# ---------------------------------------------------------
# Continent-level dataset (population-weighted aggregates)
# ---------------------------------------------------------

gadm_df_aqli_continent <- reactive({
  continent_weighted[
    continent %in% rv_continent_aq$continent_aq &
      (population >= input$pop_range[1]) &
      (population <= input$pop_range[2])
  ]
})


# ---------------------------------------------------------
# Select dataset dynamically depending on selected level
# This controls which geographic level the dashboard uses
# ---------------------------------------------------------

selected_df <- reactive({
  
  req(input$level_choice)   # wait until level_choice input exists
  level <- input$level_choice
  
  if(level == "Continent") continent_weighted
  else if(level == "AQLI Region") aqli_region_weighted
  else if(level == "Country") gadm0_aqli
  else if(level == "State") gadm1_aqli
  else if(level == "District") gadm2_aqli
  else continent_weighted  # fallback default
})


# ---------------------------------------------------------
# Debugging observer to print selected level
# ---------------------------------------------------------

observe({
  print("Selected Items")
  print(input$level_choice)
})


# ---------------------------------------------------------
# Dynamically update population slider range
# based on the selected geographic level dataset
# ---------------------------------------------------------

observeEvent(selected_df(), {
  
  df <- selected_df()
  
  # Define new slider limits
  new_min  <- 0
  new_max  <- ceiling(max(df$population, na.rm = TRUE))
  new_value <- c(new_min, new_max)
  
  updateSliderInput(
    session,
    inputId = "pop_range",
    min = new_min,
    max = new_max,
    value = new_value,
    step = 1000000   # step size for slider movement
  )
})


output$bar_aqli_comp_pm <- renderHighchart({
  
  if(input$aqli_comp_choice== "pm_comp_25"){
  
  colors <- c("#FF8C00", "#1F4E79", "#FFB84D", "#336699", "#FF9933") # dark orange & blue shades
  title_all <- paste0( "<span style='color:maroon;'>", 
                       input$level_choice, 
                       " Wise PM2.5 concentration")
  y_axis = "PM2.5 (µg/m³)"
      
      if(input$level_choice == "Continent"){
        
      data = gadm_df_aqli_continent() %>%  filter(!is.na(pm2024_weighted)) %>% 
        select("continent", all_of(starts_with("pm"))) %>%
        setNames(c("continent", as.character(1998:2024))) %>%
        pivot_longer(!continent,names_to = "year", values_to = "PM2.5")
      
      } else if(input$level_choice == "AQLI Region"){
        
        data = gadm_df_aqli_region() %>%  filter(!is.na(pm2024_weighted)) %>% 
          select("region", all_of(starts_with("pm"))) %>%
          setNames(c("region", as.character(1998:2024))) %>%
          pivot_longer(!region,names_to = "year", values_to = "PM2.5")
        
      } else if(input$level_choice == "Country"){
        
        data = gadm_df_aqli_country() %>% filter(!is.na(pm2024)) %>% 
          select("country","continent", all_of(starts_with("pm"))) %>%
          setNames(c("country", "continent",  as.character(1998:2024))) %>%# select(-c("population", "whostandard", "natstandard"))
          pivot_longer(!c(country, continent), names_to = "year", values_to = "PM2.5")
        
        
      } else if(input$level_choice == "State"){
        
        data = gadm_df_aqli_state() %>%  filter(!is.na(pm2024)) %>% 
          select("name_1","continent", "country",all_of(starts_with("pm"))) %>%
          setNames(c( "name_1", "continent", "country",as.character(1998:2024))) %>%
          pivot_longer(!c(continent, country, name_1),names_to = "year", values_to = "PM2.5")
        
        
      } else if(input$level_choice == "District"){
        
        data = gadm_df_aqli_district() %>% filter(!is.na(pm2024)) %>% 
          select("name_2", "continent", "country", "name_1",  all_of(starts_with("pm"))) %>%
          setNames(c("name_2", "continent", "country", "name_1",  as.character(1998:2024))) %>%
          pivot_longer(!c(name_2,continent,country, name_1), names_to = "year", values_to = "PM2.5")
        
        
      } 
      
      } 
  else if(input$aqli_comp_choice == "llpp_comp"){
        
        colors <- c("#FF8C00", "#1F4E79", "#FFB84D", "#336699", "#FF9933") # dark orange & blue shades

        title_all <- paste0( "<span style='color:maroon;'>", 
                             input$level_choice, 
                             " Wise Life Year Loss Per Person(WHO)")
        
        
        y_axis = "Year"
        
    
    if(input$level_choice == "Continent"){
      
      data = gadm_df_aqli_continent() %>%  filter(!is.na(pm2024_weighted)) %>% 
            select("continent", all_of(starts_with("llpp_who"))) %>%
            setNames(c("continent", as.character(1998:2024))) %>%
            pivot_longer(!continent, names_to = "year", values_to = "PM2.5")
      
    } else if(input$level_choice == "AQLI Region"){
      
      data = gadm_df_aqli_region() %>%  filter(!is.na(pm2024_weighted)) %>% 
        select("region", all_of(starts_with("llpp_who"))) %>%
        setNames(c("region", as.character(1998:2024))) %>%
        pivot_longer(!region,names_to = "year", values_to = "PM2.5")
      
    } else if(input$level_choice == "Country"){
      
      data = gadm_df_aqli_country() %>%  filter(!is.na(pm2024)) %>% 
        select("country","continent", all_of(starts_with("llpp_who"))) %>%
        setNames(c("country", "continent",  as.character(1998:2024))) %>%# select(-c("population", "whostandard", "natstandard"))
        pivot_longer(!c(country,continent), names_to = "year", values_to = "PM2.5")
      
      
    } else if(input$level_choice == "State"){
      
      data = gadm_df_aqli_state() %>%  filter(!is.na(pm2024)) %>% 
        select("name_1","continent", "country", all_of(starts_with("llpp_who"))) %>%
        setNames(c( "name_1", "continent", "country",as.character(1998:2024))) %>%
        pivot_longer(!c(continent, country, name_1),names_to = "year", values_to = "PM2.5")
      
      
    } else if(input$level_choice == "District"){
      
      data = gadm_df_aqli_district() %>%  filter(!is.na(pm2024)) %>% 
        select("name_2", "continent", "country", "name_1",  all_of(starts_with("llpp_who"))) %>%
        setNames(c("name_2", "continent", "country", "name_1",  as.character(1998:2024))) %>%
        pivot_longer(!c(name_2,continent,country, name_1), names_to = "year", values_to = "PM2.5")
      
      
    } 
    
    
    
  } 
  else if(input$aqli_comp_choice == "nat_llpp_comp"){
    colors <- c("#FF8C00", "#1F4E79", "#FFB84D", "#336699", "#FF9933") # dark orange & blue shades
    title_all <- paste0( "<span style='color:maroon;'>", 
                         input$level_choice, 
                         " Wise Life Year Loss Per Person (Nat.)")
   

    y_axis = "Year"
    
    
    if(input$level_choice == "Continent"){
      
      data = gadm_df_aqli_continent() %>%  filter(!is.na(pm2024_weighted)) %>% 
        select("continent", all_of(starts_with("llpp_nat"))) %>%
        setNames(c("continent", as.character(1998:2024))) %>%
        pivot_longer(!continent,names_to = "year", values_to = "PM2.5")
      
    } else if(input$level_choice == "AQLI Region"){
      
      data = gadm_df_aqli_region() %>%  filter(!is.na(pm2024_weighted)) %>% 
        select("region", all_of(starts_with("llpp_nat"))) %>%
        setNames(c("region", as.character(1998:2024))) %>%
        pivot_longer(!region,names_to = "year", values_to = "PM2.5")
      
    } else if(input$level_choice == "Country"){
      
      data = gadm_df_aqli_country() %>%  filter(!is.na(pm2024)) %>% 
        select("country","continent", all_of(starts_with("llpp_nat"))) %>%
        setNames(c("country", "continent",  as.character(1998:2024))) %>%# select(-c("population", "whostandard", "natstandard"))
        pivot_longer(!c(country,continent), names_to = "year", values_to = "PM2.5")
      
      
    } else if(input$level_choice == "State"){
      
      data = gadm_df_aqli_state() %>%  filter(!is.na(pm2024)) %>% 
        select("name_1","continent", "country",all_of(starts_with("llpp_nat"))) %>%
        setNames(c( "name_1", "continent", "country",as.character(1998:2024))) %>%
        pivot_longer(!c(continent, country, name_1),names_to = "year", values_to = "PM2.5")
      
      
    } else if(input$level_choice == "District"){
      
      data = gadm_df_aqli_district() %>%  filter(!is.na(pm2024)) %>% 
        select("name_2", "continent", "country", "name_1",  all_of(starts_with("llpp_nat"))) %>%
        setNames(c("name_2", "continent", "country", "name_1",  as.character(1998:2024))) %>%
        pivot_longer(!c(name_2,continent,country, name_1), names_to = "year", values_to = "PM2.5")
      
      
    }
    
  }
  
  data <- data %>% filter(year %in% input$year_aq)
  
  # X variable name
  x_var <- switch(input$level_choice,
                  "Continent" = "continent",
                  "AQLI Region"    = "region",
                  "Country"   = "country",
                  "State"     = "name_1",
                  "District"  = "name_2"
  )
  
  years <- sort(unique(data$year))
  
  # ---- Choose reference order (latest year) ----
  ref_year <- max(years)
  
  category_order <- data %>%
    filter(year == ref_year) %>%
    arrange(PM2.5) %>%          # increasing order of PM2.5
    pull(!!sym(x_var))
  
  
  # if (input$use_bar_col) {
  #   
  #   type = "line"
  #   } else{
  #     type = "column"
  #   
  # }
  
  # AQLI Themed Highchart: Multi-Year Column Chart
  hc <- highchart() %>%
    hc_chart(
      type = 'column',
      event= list(
        selection = JS_Zoom
      ),
      zoomType= "x",
     # backgroundColor = "#FFFFFF",
     style = list(fontFamily = "Lato, sans-serif")
    ) %>%
    hc_title(
      text = title_all,
      align = "center",
      style = list(
        color = "black",   # AQLI Dark Blue
        fontSize = "18px",
        fontWeight = "bold"
      )
    ) %>%
    hc_xAxis(
      categories = category_order,
      title = list(
        text = input$level_choice,
        style = list(color = "#333333", fontSize = "13px")
      ),
      labels = list(
        style = list(color = "#4A4A4A", fontSize = "12px")
       # rotation = -45
      ),
      lineColor = "#CCCCCC",
      tickColor = "#CCCCCC"
    ) %>%
    hc_yAxis(
      title = list(
        text = y_axis,
        style = list(color = "#333333", fontSize = "13px")
      ),
      gridLineColor = "#E0E0E0",
      labels = list(style = list(color = "#4A4A4A", fontSize = "12px"))
    ) %>%
    hc_tooltip(
      shared = TRUE,
      backgroundColor = "rgba(255, 255, 255, 0.95)",
      borderColor = "#CCCCCC",
      style = list(fontSize = "12px"),
      headerFormat = '<b style="font-size:13px;">{point.key}</b><br>',
      pointFormat = '<span style="color:{series.color}">\u25CF</span> {series.name}: <b>{point.y:.1f}</b><br>'
    ) %>%
    hc_legend(
      enabled = TRUE,
      align = "center",
      verticalAlign = "bottom",
      itemStyle = list(color = "#416891", fontWeight = "normal"),
      itemHoverStyle = list(color = "#ffa521")
    ) %>%
    hc_plotOptions(
      column = list(
        grouping = TRUE,
        shadow = FALSE,
        borderWidth = 0,
        pointPadding = 0.1,
        dataLabels = list(
          enabled = TRUE,
          format = "{y:.1f}",
          style = list(color = "#4A4A4A", fontSize = "11px")
        )
      )
    ) %>%
    hc_colors(c("#416891", "#F26B38", "#69B3A2", "#FFB81C", "#7D8CA3")) %>%  # AQLI palette
    hc_exporting(enabled = TRUE) %>%
    hc_credits(
      enabled = TRUE,
      text = "Air Quality Life Index (AQLI)",
      href = "https://aqli.epic.uchicago.edu",
      style = list(color = "#7D8CA3", fontSize = "11px")
    )
  
  # Add series for each year in loop
  for (yr in years) {
    df_year <- data %>%
      filter(year == yr) %>%
      right_join(tibble(!!sym(x_var) := category_order), by = x_var) %>%
      arrange(factor(!!sym(x_var), levels = category_order))
    
    hc <- hc %>%
      hc_add_series(
        name = as.character(yr),
        data = df_year$PM2.5,
        tooltip = list(valueSuffix = " µg/m³")
      )
  }
  
  hc
})

output$yr_wse_line_aqli <- renderHighchart({
  
  if(input$aqli_comp_choice== "pm_comp_25"){
    
    colors <- c("#FF8C00", "#1F4E79", "#FFB84D", "#336699", "#FF9933") # dark orange & blue shades
    title_all <- paste0( "<span style='color:maroon;'>", 
                         input$level_choice, 
                         " Wise PM2.5 concentration")
    y_axis = "PM2.5 (µg/m³)"
    
    if(input$level_choice == "Continent"){
      
      data = gadm_df_aqli_continent() %>%  filter(!is.na(pm2024_weighted)) %>% 
        select("continent", all_of(starts_with("pm"))) %>%
        setNames(c("continent", as.character(1998:2024))) %>%
        pivot_longer(!continent,names_to = "year", values_to = "PM2.5")
      
    } else if(input$level_choice == "AQLI Region"){
      
      data = gadm_df_aqli_region() %>%  filter(!is.na(pm2024_weighted)) %>% 
        select("region", all_of(starts_with("pm"))) %>%
        setNames(c("region", as.character(1998:2024))) %>%
        pivot_longer(!region,names_to = "year", values_to = "PM2.5")
      
    } else if(input$level_choice == "Country"){
      
      data = gadm_df_aqli_country() %>%  filter(!is.na(pm2024)) %>% 
        select("country","continent", all_of(starts_with("pm"))) %>%
        setNames(c("country", "continent",  as.character(1998:2024))) %>%# select(-c("population", "whostandard", "natstandard"))
        pivot_longer(!c(country, continent), names_to = "year", values_to = "PM2.5")
      
      
    } else if(input$level_choice == "State"){
      
      data = gadm_df_aqli_state() %>%  filter(!is.na(pm2024)) %>% 
        select("name_1","continent", "country",all_of(starts_with("pm"))) %>%
        setNames(c( "name_1", "continent", "country",as.character(1998:2024))) %>%
        pivot_longer(!c(continent, country, name_1),names_to = "year", values_to = "PM2.5")
      
      
    } else if(input$level_choice == "District"){
      
      data = gadm_df_aqli_district() %>%  filter(!is.na(pm2024)) %>% 
        select("name_2", "continent", "country", "name_1",  all_of(starts_with("pm"))) %>%
        setNames(c("name_2", "continent", "country", "name_1",  as.character(1998:2024))) %>%
        pivot_longer(!c(name_2,continent,country, name_1), names_to = "year", values_to = "PM2.5")
      
      
    } 
    
  } 
  else if(input$aqli_comp_choice == "llpp_comp"){
    
    colors <- c("#FF8C00", "#1F4E79", "#FFB84D", "#336699", "#FF9933") # dark orange & blue shades
    
    title_all <- paste0( "<span style='color:maroon;'>", 
                         input$level_choice, 
                         " Wise Life Year Loss Per Person(WHO)")
    
    
    y_axis = "Year"
    
    
    if(input$level_choice == "Continent"){
      
      data = gadm_df_aqli_continent() %>%  filter(!is.na(pm2024_weighted)) %>% 
        select("continent", all_of(starts_with("llpp_who"))) %>%
        setNames(c("continent", as.character(1998:2024))) %>%
        pivot_longer(!continent, names_to = "year", values_to = "PM2.5")
      
    } else if(input$level_choice == "AQLI Region"){
       
      data = gadm_df_aqli_region() %>%  filter(!is.na(pm2024_weighted)) %>% 
        select("region", all_of(starts_with("llpp_who"))) %>%
        setNames(c("region", as.character(1998:2024))) %>%
        pivot_longer(!region,names_to = "year", values_to = "PM2.5")
      
    } else if(input$level_choice == "Country"){
      
      data = gadm_df_aqli_country() %>%  filter(!is.na(pm2024)) %>% 
        select("country","continent", all_of(starts_with("llpp_who"))) %>%
        setNames(c("country", "continent",  as.character(1998:2024))) %>%# select(-c("population", "whostandard", "natstandard"))
        pivot_longer(!c(country,continent), names_to = "year", values_to = "PM2.5")
      
      
    } else if(input$level_choice == "State"){
      
      data = gadm_df_aqli_state() %>%
        select("name_1","continent", "country", all_of(starts_with("llpp_who"))) %>%
        setNames(c( "name_1", "continent", "country",as.character(1998:2024))) %>%
        pivot_longer(!c(continent, country, name_1),names_to = "year", values_to = "PM2.5")
      
      
    } else if(input$level_choice == "District"){
      
      data = gadm_df_aqli_district() %>%
        select("name_2", "continent", "country", "name_1",  all_of(starts_with("llpp_who"))) %>%
        setNames(c("name_2", "continent", "country", "name_1",  as.character(1998:2024))) %>%
        pivot_longer(!c(name_2,continent,country, name_1), names_to = "year", values_to = "PM2.5")
      
      
    } 
    
    
    
  } 
  else if(input$aqli_comp_choice == "nat_llpp_comp"){
    colors <- c("#FF8C00", "#1F4E79", "#FFB84D", "#336699", "#FF9933") # dark orange & blue shades
    title_all <- paste0( "<span style='color:maroon;'>", 
                         input$level_choice, 
                         " Wise Life Year Loss Per Person (Nat.)")
    
    
    y_axis = "Year"
    
    
    if(input$level_choice == "Continent"){
      
      data = gadm_df_aqli_continent() %>%  filter(!is.na(pm2024_weighted)) %>% 
        select("continent", all_of(starts_with("llpp_nat"))) %>%
        setNames(c("continent", as.character(1998:2024))) %>%
        pivot_longer(!continent,names_to = "year", values_to = "PM2.5")
      
    } else if(input$level_choice == "AQLI Region"){
      
      data = gadm_df_aqli_region() %>%  filter(!is.na(pm2024_weighted)) %>% 
        select("region", all_of(starts_with("llpp_nat"))) %>%
        setNames(c("region", as.character(1998:2024))) %>%
        pivot_longer(!region,names_to = "year", values_to = "PM2.5")
      
    } else if(input$level_choice == "Country"){
      
      data = gadm_df_aqli_country() %>%  filter(!is.na(pm2024)) %>% 
        select("country","continent", all_of(starts_with("llpp_nat"))) %>%
        setNames(c("country", "continent",  as.character(1998:2024))) %>%# select(-c("population", "whostandard", "natstandard"))
        pivot_longer(!c(country,continent), names_to = "year", values_to = "PM2.5")
      
      
    } else if(input$level_choice == "State"){
      
      data = gadm_df_aqli_state() %>%  filter(!is.na(pm2024)) %>% 
        select("name_1","continent", "country",all_of(starts_with("llpp_nat"))) %>%
        setNames(c( "name_1", "continent", "country",as.character(1998:2024))) %>%
        pivot_longer(!c(continent, country, name_1),names_to = "year", values_to = "PM2.5")
      
      
    } else if(input$level_choice == "District"){
      
      data = gadm_df_aqli_district() %>%  filter(!is.na(pm2024)) %>% 
        select("name_2", "continent", "country", "name_1",  all_of(starts_with("llpp_nat"))) %>%
        setNames(c("name_2", "continent", "country", "name_1",  as.character(1998:2024))) %>%
        pivot_longer(!c(name_2,continent,country, name_1), names_to = "year", values_to = "PM2.5")
      
      
    }
    
  }
  
  # X variable name based on level choice
  x_var <- switch(input$level_choice,
                  "Continent" = "continent",
                  "AQLI Region" = "region",
                  "Country" = "country",
                  "State" = "name_1",
                  "District" = "name_2"
  )
  
  # Years on X-axis
  years <- sort(unique(data$year))
  
  # Prepare dataset for trend chart
  trend_data <- data %>%
    arrange(!!sym(x_var), year)
  
  # Unique categories (series)
  categories <- unique(trend_data[[x_var]])
  
  # Highchart: Year-wise trend
  hc <- highchart() %>%
    hc_chart(type = "line",
             style = list(fontFamily = "Lato, sans-serif")) %>%
    hc_title(
      text = title_all,
      align = "center",
      style = list(color = "black", fontSize = "18px", fontWeight = "bold")
    ) %>%
    hc_xAxis(
      categories = years,
      title = list(text = "Year",
                   style = list(color = "#333333", fontSize = "13px")),
      labels = list(style = list(color = "#4A4A4A", fontSize = "12px")),
      lineColor = "#CCCCCC",
      tickColor = "#CCCCCC"
    ) %>%
    hc_yAxis(
      title = list(text = y_axis,
                   style = list(color = "#333333", fontSize = "13px")),
      gridLineColor = "#E0E0E0",
      labels = list(style = list(color = "#4A4A4A", fontSize = "12px"))
    ) %>%
    hc_tooltip(
      shared = TRUE,
      backgroundColor = "rgba(255,255,255,0.95)",
      borderColor = "#CCCCCC",
      style = list(fontSize = "12px"),
      headerFormat = '<b>Year: {point.key}</b><br>',
      pointFormat = '<span style="color:{series.color}">\u25CF</span> {series.name}: <b>{point.y:.1f}</b><br>'
    ) %>%
    hc_legend(
      enabled = TRUE,
      align = "center",
      verticalAlign = "bottom",
      itemStyle = list(color = "#416891"),
      itemHoverStyle = list(color = "#ffa521")
    ) %>%
    hc_colors(c("#416891", "#F26B38", "#69B3A2", "#FFB81C", "#7D8CA3",
                "#8C564B", "#17BECF", "#9467BD")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_credits(
      enabled = TRUE,
      text = "Air Quality Life Index (AQLI)",
      href = "https://aqli.epic.uchicago.edu",
      style = list(color = "#7D8CA3", fontSize = "11px")
    )
  
  # ---- Add series (each category = 1 line) ----
  for (cat in categories) {
    df_cat <- trend_data %>%
      filter(!!sym(x_var) == cat) %>%
      arrange(year)
    
    hc <- hc %>% hc_add_series(
      name = cat,
      data = df_cat$PM2.5,
      tooltip = list(valueSuffix = " µg/m³"),
      marker = list(enabled = TRUE)
    )
  }
  
  hc
  
})

############## Added 
output$pop_below_above_who <- renderHighchart({
  
  if(input$use_nnum_perc){
    
    
    y_axis <- "PM2.5 (µg/m³)"
    title_all <- paste0("<span style='color:maroon;'>",
                        input$level_choice,
                        " Wise Population Exposure to PM2.5</span>")  # Updated to match plot
    
    # Common select: include Region for all cases, only PM columns
    print("---------------------------------------------------------")
    
    # print(gadm2_data_filter())
    base_select <- gadm2_aqli  %>%  select(name_2, population, continent, country, name_1, region, starts_with("pm")) 
   
    # Pivot PM only
    base_data <- base_select %>%
      pivot_longer(
        cols = starts_with("pm"),
        names_to = "year_str",
        names_prefix = "pm",
        values_to = "PM2.5"
      ) %>%
      mutate(
        year = as.numeric(year_str),
        category = ifelse(PM2.5 > who_guideline, "Above WHO", "Below WHO")
      ) %>%
      filter(!is.na(category)) 
    
    # Now group and summarise based on level_choice
    if (input$level_choice == "Continent") {
      data <- base_data %>% filter( (continent  %in%  input$continent_aq ) ) %>% 
        group_by(continent, year, category) %>%
        summarise(
          tot_pop = sum(population, na.rm = TRUE),
          PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
          .groups = "drop"
        ) %>%  group_by(continent, year) %>%
        mutate(
          population = sum(tot_pop, na.rm = TRUE),
          pct = (tot_pop / population) * 100
        ) %>%  filter( (population >= input$pop_range[1] ) &
                         (population <= input$pop_range[2] )) %>% 
        ungroup()
      
    } else if (input$level_choice == "AQLI Region") {
      data <- base_data %>%  filter(   (continent %in%  input$continent_aq ) &
                                         (region    %in%  input$region_aq    )) %>% 
        group_by(region, year, category) %>%
        summarise(
          tot_pop = sum(population, na.rm = TRUE),
          PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
          .groups = "drop"
        ) %>% 
        group_by(region, year) %>%
        mutate(
          population = sum(tot_pop, na.rm = TRUE),
          pct = (tot_pop / population) * 100
        ) %>% filter( (population >= input$pop_range[1] ) &
                        (population <= input$pop_range[2] )) %>% 
        ungroup()
      
    } else if (input$level_choice == "Country") {
      data <- base_data %>%   filter(   (continent %in%  input$continent_aq ) &
                                          (region    %in%  input$region_aq    ) &
                                          (country   %in%  input$country_aq   ) ) %>% 
        group_by(country, year, category) %>%
        summarise(
          tot_pop = sum(population, na.rm = TRUE),
          PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
          .groups = "drop"
        ) %>% 
        group_by(country, year) %>%
        mutate(
          population = sum(tot_pop, na.rm = TRUE),
          pct = (tot_pop / population) * 100
        ) %>% filter( (population >= input$pop_range[1] ) &
                        (population <= input$pop_range[2] )) %>% 
        ungroup()
      
    } else if (input$level_choice == "State") {
      data <- base_data %>%   filter(   (continent %in%  input$continent_aq ) &
                                          (region    %in%  input$region_aq    ) &
                                          (country   %in%  input$country_aq   ) &
                                          (name_1    %in%  input$state_aq     ) ) %>% 
        group_by(country, name_1, year, category) %>%
        summarise(
          tot_pop = sum(population, na.rm = TRUE),
          PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
          .groups = "drop"
        ) %>% 
        group_by(country, name_1, year) %>%
        mutate(
          population = sum(tot_pop, na.rm = TRUE),
          pct = (tot_pop / population) * 100
        ) %>% filter( (population >= input$pop_range[1] ) &
                         (population <= input$pop_range[2] )) %>% 
        ungroup()
      
    } else if (input$level_choice == "District") {
      data <- base_data %>%  filter(   (continent %in%  input$continent_aq ) &
                                         (region    %in%  input$region_aq    ) &
                                         (country   %in%  input$country_aq   ) &
                                         (name_1    %in%  input$state_aq     ) &
                                         (name_2    %in%  input$district_aq  )) %>% 
        
        group_by(country, name_1, name_2, year, category) %>%
        summarise(
          tot_pop = sum(population, na.rm = TRUE),
          PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
          .groups = "drop"
        ) %>% 
        group_by(country, name_1, name_2, year) %>%
        mutate(
          population = sum(tot_pop, na.rm = TRUE),
          pct = (tot_pop / population) * 100
        ) %>% filter( (population >= input$pop_range[1] ) &
                         (population <= input$pop_range[2] )) %>% 
        ungroup()
    }
    
    # Filter by selected years
    data <- data %>% filter(year %in% input$year_aq)
    
    # X variable selector
    x_var <- switch(input$level_choice,
                    "Continent" = "continent",
                    "AQLI Region" = "region",
                    "Country" = "country",
                    "State" = "name_1",
                    "District" = "name_2"
    )
    
    years <- sort(unique(data$year))
    
    # Reference ordering from latest year
    ref_year <- max(years)
    category_order <- data %>%
      filter(year == ref_year) %>%
      group_by(!!sym(x_var)) %>%
      summarise(tot_pop = sum(tot_pop), .groups = "drop") %>%
      arrange(tot_pop) %>%
      pull(!!sym(x_var))
    
    # -------------------------------
    # ⚡ Number formatting JS function
    # -------------------------------
    js_format <- JS("
function() {
    let v = this.value;
    if (v >= 1e9) return (v/1e9).toFixed(2) + 'B';
    if (v >= 1e6) return (v/1e6).toFixed(2) + 'M';
    if (v >= 1e3) return (v/1e3).toFixed(2) + 'K';
    return v;
}
")
    
    js_datalabel <- JS("
function() {
    let v = this.y;
    if (v >= 1e9) return (v/1e9).toFixed(2) + 'B';
    if (v >= 1e6) return (v/1e6).toFixed(2) + 'M';
    if (v >= 1e3) return (v/1e3).toFixed(2) + 'K';
    return v;
}
")
    
    js_tooltip <- JS("
                  function() {
                      let v = this.y;
                      if (v >= 1e9) v = (v/1e9).toFixed(2) + 'B';
                      else if (v >= 1e6) v = (v/1e6).toFixed(2) + 'M';
                      else if (v >= 1e3) v = (v/1e3).toFixed(2) + 'K';
                      return '<b>' + this.series.stackKey.replace('stack','') +
                             '</b><br>' +
                             '<span style=\"color:' + this.color + '\">●</span> ' +
                             this.series.name + ': <b>' + v + '</b>';
                  }
                  ")
    
    hc_theme_aqli <- hc_theme(
      chart = list(
        backgroundColor = "#ffffff",
        style = list(fontFamily = "Montserrat, Helvetica, Arial, sans-serif")
      ))
    # -------------------------------
    # ⚡ Base Highchart
    # -------------------------------
    hc <- highchart() %>%
      hc_chart(type = "column",    event= list(
        selection = JS_Zoom
      ),
      zoomType= "x", style = list(fontFamily = "Lato, sans-serif")) %>%
      hc_title(
        text = title_all,
        align = "center",
        style = list(color = "black", fontSize = "18px", fontWeight = "bold")
      ) %>%
      hc_xAxis(
        categories = category_order,
        title = list(
          text = input$level_choice,
          style = list(color = "#333333", fontSize = "13px")
        ),
        labels = list(style = list(color = "#4A4A4A", fontSize = "12px")),
        lineColor = "#CCCCCC",
        tickColor = "#CCCCCC"
      ) %>%
      hc_yAxis(
        # type = "logarithmic",
        title = list(
          text = "Population Percentage(%)",
          style = list(color = "#333333", fontSize = "13px")
        ),
        labels = list(
          formatter = js_format,
          style = list(color = "#4A4A4A", fontSize = "12px")
        ),
        minorTickInterval = 0.1,
        gridLineColor = "#E0E0E0"
      ) %>%         
      hc_tooltip(
        shared = FALSE,
        useHTML = TRUE,
        formatter = js_tooltip,
        backgroundColor = "rgba(255,255,255,0.95)",
        borderColor = "#CCCCCC",
        style = list(fontSize = "12px")
      ) %>%
      hc_legend(
        enabled = TRUE,
        align = "center",
        verticalAlign = "bottom",
        itemStyle = list(color = "#416891", fontWeight = "normal"),
        itemHoverStyle = list(color = "#ffa521")
      ) %>%
      hc_plotOptions(
        column = list(
          stacking = "normal",
          borderWidth = 0,
          pointPadding = 0.05,
          groupPadding = 0.05
        ),
        series = list(
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("
    function() {
      const n = this.y;
      if (n >= 1e9) {
        return (n/1e9).toFixed(1) + 'B';
      } else if (n >= 1e6) {
        return (n/1e6).toFixed(1) + 'M';
      } else if (n >= 1e3) {
        return (n/1e3).toFixed(1) + 'K';
      } else {
        return n.toFixed(1);
      }
    }
  "),
            style = list(
              color = "black",        # ← label color black
              textOutline = "none",
              fontSize = "11px",
              fontWeight = "bold"
            )
          )
        )
      ) %>%
      hc_colors(c("#69B3A2", "#F26B38")) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_credits(
        enabled = TRUE,
        text = "Air Quality Life Index (AQLI)",
        href = "https://aqli.epic.uchicago.edu",
        style = list(color = "#7D8CA3", fontSize = "11px")
      )
    
    # ---------------------------------
    # ⚡ Add series for each year
    # Stacked inside year, grouped by year
    # ---------------------------------
    for (yr in years) {
      
      # BELOW WHO
      df_below <- data %>%
        filter(year == yr, category == "Below WHO") %>%
        right_join(tibble(!!sym(x_var) := category_order), by = x_var) %>%
        arrange(factor(!!sym(x_var), levels = category_order)) %>%
        mutate(pct = replace_na(pct, 0)) %>%
        pull(pct)
      
      hc <- hc %>%
        hc_add_series(
          name = "Below WHO",
          data = df_below,
          stack = paste0("stack", yr),
          showInLegend = (yr == years[1]),
          color = "#69B3A2"
        )
      
      # ABOVE WHO
      df_above <- data %>%
        filter(year == yr, category == "Above WHO") %>%
        right_join(tibble(!!sym(x_var) := category_order), by = x_var) %>%
        arrange(factor(!!sym(x_var), levels = category_order)) %>%
        mutate(pct = replace_na(pct, 0)) %>%
        pull(pct)
      
      hc <- hc %>%
        hc_add_series(
          name = "Above WHO",
          data = df_above,
          stack = paste0("stack", yr),
          showInLegend = (yr == years[1]),
          color = "#F26B38"
        )
    }
    
    hc
    
    
    
  }else {
    
  y_axis <- "PM2.5 (µg/m³)"
  title_all <- paste0("<span style='color:maroon;'>",
                      input$level_choice,
                      " Wise Population Exposure to PM2.5</span>")  # Updated to match plot
  
  # Common select: include Region for all cases, only PM columns
  print("---------------------------------------------------------")
  
  # print(gadm2_data_filter())
  base_select <- gadm2_aqli  %>%  select(name_2, population, continent, country, name_1, region, starts_with("pm")) 

  # Pivot PM only
  base_data <- base_select %>%
    pivot_longer(
      cols = starts_with("pm"),
      names_to = "year_str",
      names_prefix = "pm",
      values_to = "PM2.5"
    ) %>%
    mutate(
      year = as.numeric(year_str),
      category = ifelse(PM2.5 > who_guideline, "Above WHO", "Below WHO")
    ) %>%
    filter(!is.na(category)) 
  
  # Now group and summarise based on level_choice
  if (input$level_choice == "Continent") {
    data <- base_data %>% filter( (continent  %in%  input$continent_aq ) ) %>% 
      group_by(continent, year, category) %>%
      summarise(
        tot_pop = sum(population, na.rm = TRUE),
        PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
        .groups = "drop"
      )   %>%  group_by(continent, year) %>%
        mutate(
        population = sum(tot_pop, na.rm = TRUE),
        pct = (tot_pop / population) * 100
      ) %>% filter( (population >= input$pop_range[1] ) &
                      (population <= input$pop_range[2] )) %>% 
      ungroup()
      
  } else if (input$level_choice == "AQLI Region") {
    data <- base_data %>%  filter(   (continent %in%  input$continent_aq ) &
                                       (region    %in%  input$region_aq    )) %>% 
      group_by(region, year, category) %>%
      summarise(
        tot_pop = sum(population, na.rm = TRUE),
        PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
        .groups = "drop"
      ) %>%  group_by(region, year) %>%
      mutate(
        population = sum(tot_pop, na.rm = TRUE),
        pct = (tot_pop / population) * 100
      ) %>% filter( (population >= input$pop_range[1] ) &
                      (population <= input$pop_range[2] )) %>% 
      ungroup()
    
  } else if (input$level_choice == "Country") {
    data <- base_data %>%   filter(   (continent %in%  input$continent_aq ) &
                                        (region    %in%  input$region_aq    ) &
                                        (country   %in%  input$country_aq   ) ) %>% 
      group_by(country, year, category) %>%
      summarise(
        tot_pop = sum(population, na.rm = TRUE),
        PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
        .groups = "drop"
      ) %>%  group_by(country, year) %>%
      mutate(
        population = sum(tot_pop, na.rm = TRUE),
        pct = (tot_pop / population) * 100
      ) %>% filter( (population >= input$pop_range[1] ) &
                      (population <= input$pop_range[2] )) %>% 
      ungroup()
    
  } else if (input$level_choice == "State") {
    data <- base_data %>%   filter(   (continent %in%  input$continent_aq ) &
                                        (region    %in%  input$region_aq    ) &
                                        (country   %in%  input$country_aq   ) &
                                        (name_1    %in%  input$state_aq     ) ) %>% 
      group_by(country, name_1, year, category) %>%
      summarise(
        tot_pop = sum(population, na.rm = TRUE),
        PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
        .groups = "drop"
      ) %>%  group_by(country, name_1, year) %>%
      mutate(
        population = sum(tot_pop, na.rm = TRUE),
        pct = (tot_pop / population) * 100
      ) %>% filter( (population >= input$pop_range[1] ) &
                      (population <= input$pop_range[2] )) %>% 
      ungroup()
    
  } else if (input$level_choice == "District") {
    data <- base_data %>%  filter(   (continent %in%  input$continent_aq ) &
                                       (region    %in%  input$region_aq    ) &
                                       (country   %in%  input$country_aq   ) &
                                       (name_1    %in%  input$state_aq     ) &
                                       (name_2    %in%  input$district_aq  )) %>% 
      
      group_by(country, name_1, name_2, year, category) %>%
      summarise(
        tot_pop = sum(population, na.rm = TRUE),
        PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
        .groups = "drop"
      ) %>%  group_by(country, name_1, name_2, year) %>%
      mutate(
        population = sum(tot_pop, na.rm = TRUE),
        pct = (tot_pop / population) * 100
      ) %>% filter( (population >= input$pop_range[1] ) &
                      (population <= input$pop_range[2] )) %>% 
      ungroup()
  }
  
  # Filter by selected years
  data <- data %>% filter(year %in% input$year_aq)
  
  # X variable selector
  x_var <- switch(input$level_choice,
                  "Continent" = "continent",
                  "AQLI Region" = "region",
                  "Country" = "country",
                  "State" = "name_1",
                  "District" = "name_2"
  )
  
  years <- sort(unique(data$year))
  
  # Reference ordering from latest year
  ref_year <- max(years)
  category_order <- data %>%
    filter(year == ref_year) %>%
    group_by(!!sym(x_var)) %>%
    summarise(tot_pop = sum(tot_pop), .groups = "drop") %>%
    arrange(tot_pop) %>%
    pull(!!sym(x_var))
  
  # -------------------------------
  # ⚡ Number formatting JS function
  # -------------------------------
  js_format <- JS("
function() {
    let v = this.value;
    if (v >= 1e9) return (v/1e9).toFixed(2) + 'B';
    if (v >= 1e6) return (v/1e6).toFixed(2) + 'M';
    if (v >= 1e3) return (v/1e3).toFixed(2) + 'K';
    return v;
}
")
  
  js_datalabel <- JS("
function() {
    let v = this.y;
    if (v >= 1e9) return (v/1e9).toFixed(2) + 'B';
    if (v >= 1e6) return (v/1e6).toFixed(2) + 'M';
    if (v >= 1e3) return (v/1e3).toFixed(2) + 'K';
    return v;
}
")
  
  js_tooltip <- JS("
                  function() {
                      let v = this.y;
                      if (v >= 1e9) v = (v/1e9).toFixed(2) + 'B';
                      else if (v >= 1e6) v = (v/1e6).toFixed(2) + 'M';
                      else if (v >= 1e3) v = (v/1e3).toFixed(2) + 'K';
                      return '<b>' + this.series.stackKey.replace('stack','') +
                             '</b><br>' +
                             '<span style=\"color:' + this.color + '\">●</span> ' +
                             this.series.name + ': <b>' + v + '</b>';
                  }
                  ")
                    
  # -------------------------------
  # ⚡ Base Highchart
  # -------------------------------
  hc <- highchart() %>%
    hc_chart(type = "column",    event= list(
      selection = JS_Zoom
    ),
    zoomType= "x", style = list(fontFamily = "Lato, sans-serif")) %>%
    hc_title(
      text = title_all,
      align = "center",
      style = list(color = "black", fontSize = "18px", fontWeight = "bold")
    ) %>%
    hc_xAxis(
      categories = category_order,
      title = list(
        text = input$level_choice,
        style = list(color = "#333333", fontSize = "13px")
      ),
      labels = list(style = list(color = "#4A4A4A", fontSize = "12px")),
      lineColor = "#CCCCCC",
      tickColor = "#CCCCCC"
    ) %>%
    hc_yAxis(
     # type = "logarithmic",
      title = list(
        text = "Population",
        style = list(color = "#333333", fontSize = "13px")
      ),
      labels = list(
        formatter = js_format,
        style = list(color = "#4A4A4A", fontSize = "12px")
      ),
      minorTickInterval = 0.1,
      gridLineColor = "#E0E0E0"
    ) %>%
    hc_tooltip(
      shared = FALSE,
      useHTML = TRUE,
      formatter = js_tooltip,
      backgroundColor = "rgba(255,255,255,0.95)",
      borderColor = "#CCCCCC",
      style = list(fontSize = "12px")
    ) %>%
    hc_legend(
      enabled = TRUE,
      align = "center",
      verticalAlign = "bottom",
      itemStyle = list(color = "#416891", fontWeight = "normal"),
      itemHoverStyle = list(color = "#ffa521")
    ) %>%
    hc_plotOptions(
      column = list(
        stacking = "normal",
        borderWidth = 0,
        pointPadding = 0.05,
        groupPadding = 0.05
      ),
      series = list(
        dataLabels = list(
          enabled = TRUE,
          formatter = JS("
    function() {
      const n = this.y;
      if (n >= 1e9) {
        return (n/1e9).toFixed(1) + 'B';
      } else if (n >= 1e6) {
        return (n/1e6).toFixed(1) + 'M';
      } else if (n >= 1e3) {
        return (n/1e3).toFixed(1) + 'K';
      } else {
        return n.toFixed(1);
      }
    }
  "),
          style = list(
            color = "black",        # ← label color black
            textOutline = "none",
            fontSize = "11px",
            fontWeight = "bold"
          )
        )
      )
    ) %>%
    hc_colors(c("#69B3A2", "#F26B38")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_credits(
      enabled = TRUE,
      text = "Air Quality Life Index (AQLI)",
      href = "https://aqli.epic.uchicago.edu",
      style = list(color = "#7D8CA3", fontSize = "11px")
    )
  
  # ---------------------------------
  # ⚡ Add series for each year
  # Stacked inside year, grouped by year
  # ---------------------------------
  for (yr in years) {
    
    # BELOW WHO
    df_below <- data %>%
      filter(year == yr, category == "Below WHO") %>%
      right_join(tibble(!!sym(x_var) := category_order), by = x_var) %>%
      arrange(factor(!!sym(x_var), levels = category_order)) %>%
      mutate(tot_pop = replace_na(tot_pop, 0)) %>%
      pull(tot_pop)
    
    hc <- hc %>%
      hc_add_series(
        name = "Below WHO",
        data = df_below,
        stack = paste0("stack", yr),
        showInLegend = (yr == years[1]),
        color = "#69B3A2"
      )
    
    # ABOVE WHO
    df_above <- data %>%
      filter(year == yr, category == "Above WHO") %>%
      right_join(tibble(!!sym(x_var) := category_order), by = x_var) %>%
      arrange(factor(!!sym(x_var), levels = category_order)) %>%
      mutate(tot_pop = replace_na(tot_pop, 0)) %>%
      pull(tot_pop)
    
    hc <- hc %>%
      hc_add_series(
        name = "Above WHO",
        data = df_above,
        stack = paste0("stack", yr),
        showInLegend = (yr == years[1]),
        color = "#F26B38"
      )
  }
  
  hc
  
  
  }
  
  # hc <- highchart() %>%
  #   hc_chart(
  #     type = "column",
  #     style = list(fontFamily = "Lato, sans-serif")
  #   ) %>%
  #   hc_title(
  #     text = title_all,
  #     align = "center",
  #     style = list(
  #       color = "black",
  #       fontSize = "18px",
  #       fontWeight = "bold"
  #     )
  #   ) %>%
  #   hc_xAxis(
  #     categories = category_order,
  #     title = list(
  #       text = input$level_choice,
  #       style = list(color = "#333333", fontSize = "13px")
  #     ),
  #     labels = list(
  #       style = list(color = "#4A4A4A", fontSize = "12px")
  #       # rotation = -45  # Uncomment for District if labels overlap
  #     ),
  #     lineColor = "#CCCCCC",
  #     tickColor = "#CCCCCC"
  #   ) %>%
  #   hc_yAxis(
  #     title = list(
  #       text = y_axis,
  #       style = list(color = "#333333", fontSize = "13px")
  #     ),
  #     gridLineColor = "#E0E0E0",
  #     labels = list(style = list(color = "#4A4A4A", fontSize = "12px"))
  #   ) %>%
  #   hc_tooltip(
  #     shared = TRUE,
  #     backgroundColor = "rgba(255, 255, 255, 0.95)",
  #     borderColor = "#CCCCCC",
  #     style = list(fontSize = "12px"),
  #     headerFormat = '<b style="font-size:13px;">{point.key}</b><br>',
  #     pointFormat = '<span style="color:{series.color}">\u25CF</span> {series.name}: <b>{point.y:.1f}</b><br>'
  #   ) %>%
  #   hc_legend(
  #     enabled = TRUE,
  #     align = "center",
  #     verticalAlign = "bottom",
  #     itemStyle = list(color = "#416891", fontWeight = "normal"),
  #     itemHoverStyle = list(color = "#ffa521")
  #   ) %>%
  #   hc_plotOptions(
  #     column = list(
  #       grouping = TRUE,  # Groups bars for categories side-by-side
  #       shadow = FALSE,
  #       borderWidth = 0,
  #       pointPadding = 0.1,
  #       groupPadding = 0.05,
  #       dataLabels = list(
  #         enabled = TRUE,
  #         format = "{y:.1f}",
  #         style = list(color = "#4A4A4A", fontSize = "11px")
  #       )
  #     )
  #   ) %>%
  #   hc_colors(c("#416891", "#F26B38", "#69B3A2", "#FFB81C", "#7D8CA3")) %>% # AQLI palette
  #   hc_exporting(enabled = TRUE) %>%
  #   hc_credits(
  #     enabled = TRUE,
  #     text = "Air Quality Life Index (AQLI)",
  #     href = "https://aqli.epic.uchicago.edu",
  #     style = list(color = "#7D8CA3", fontSize = "11px")
  #   )
  # 
  # # Add series for each year and category
  # for (yr in years) {
  #   for (cat in c("Below WHO", "Above WHO")) {
  #     df_cat <- data %>%
  #       filter(year == yr, category == cat) %>%
  #       right_join(tibble(!!sym(x_var) := category_order), by = x_var) %>%
  #       arrange(factor(!!sym(x_var), levels = category_order)) %>%
  #       mutate(PM2.5_avg = replace_na(PM2.5_avg, 0)) %>%
  #       pull(PM2.5_avg)
  #     
  #     # Fixed colors: green-ish for below, orange-red for above
  #     col_choice <- if (cat == "Below WHO") "#69B3A2" else "#F26B38"
  #     
  #     hc <- hc %>%
  #       hc_add_series(
  #         name = paste(yr, cat),
  #         data = df_cat,
  #         color = col_choice,
  #         tooltip = list(valueSuffix = " µg/m³")
  #       )
  #   }
  # }
  # 
  # hc
})


output$pop_below_above_nat <- renderHighchart({
  
  if(input$use_nnum_perc){
    
    y_axis <- "PM2.5 (µg/m³)"
    title_all <- paste0("<span style='color:maroon;'>",
                        input$level_choice,
                        " Wise Population Exposure to PM2.5</span>")  # Updated to match plot
    
    # Common select: include Region for all cases, only PM columns
    print("---------------------------------------------------------")
    
    # print(gadm2_data_filter())
    base_select <- gadm2_aqli  %>%  select(name_2, population, continent, country, name_1, region, natstandard, starts_with("pm")) 

    # Pivot PM only
    base_data <- base_select %>%
      pivot_longer(
        cols = starts_with("pm"),
        names_to = "year_str",
        names_prefix = "pm",
        values_to = "PM2.5"
      ) %>%
      mutate(
        year = as.numeric(year_str),
        category = ifelse(PM2.5 > natstandard, "Above Nat", "Below Nat")
      ) %>%
      filter(!is.na(category)) 
    
    # Now group and summarise based on level_choice
    if (input$level_choice == "Continent") {
      
      data <- base_data %>% filter( (continent  %in%  input$continent_aq ) ) %>% 
        group_by(continent, year, category) %>%
        summarise(
          tot_pop = sum(population, na.rm = TRUE),
          #PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
          .groups = "drop"
        ) %>% group_by(continent, year) %>%
        mutate(
          population = sum(tot_pop, na.rm = TRUE),
          pct = (tot_pop / population) * 100
        ) %>% filter( (population >= input$pop_range[1] ) &
                            (population <= input$pop_range[2] ))  %>% 
        ungroup()
      
    } else if (input$level_choice == "AQLI Region") {
      data <- base_data %>%  filter(   (continent %in%  input$continent_aq ) &
                                         (region    %in%  input$region_aq    )) %>% 
        group_by(region, year, category) %>%
        summarise(
          tot_pop = sum(population, na.rm = TRUE),
          #PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
          .groups = "drop"
        ) %>% 
        group_by(region, year) %>%
        mutate(
          population = sum(tot_pop, na.rm = TRUE),
          pct = (tot_pop / population) * 100
        ) %>% filter( (population >= input$pop_range[1] ) &
                        (population <= input$pop_range[2] ))  %>% 
        ungroup()
      
    } else if (input$level_choice == "Country") {
      data <- base_data %>%   filter(   (continent %in%  input$continent_aq ) &
                                          (region    %in%  input$region_aq    ) &
                                          (country   %in%  input$country_aq   ) ) %>% 
        group_by(country, year, category) %>%
        summarise(
          tot_pop = sum(population, na.rm = TRUE),
          # PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
          .groups = "drop"
        ) %>% 
        group_by(country, year) %>%
        mutate(
          population = sum(tot_pop, na.rm = TRUE),
          pct = (tot_pop / population) * 100
        ) %>% filter( (population >= input$pop_range[1] ) &
                        (population <= input$pop_range[2] ))  %>% 
        ungroup()
      
    } else if (input$level_choice == "State") {
      data <- base_data %>%   filter(   (continent %in%  input$continent_aq ) &
                                          (region    %in%  input$region_aq    ) &
                                          (country   %in%  input$country_aq   ) &
                                          (name_1    %in%  input$state_aq     ) ) %>% 
        group_by(country, name_1, year, category) %>%
        summarise(
          tot_pop = sum(population, na.rm = TRUE),
          # PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
          .groups = "drop"
        ) %>% 
        group_by(country, name_1, year) %>%
        mutate(
          population = sum(tot_pop, na.rm = TRUE),
          pct = (tot_pop / population) * 100
        ) %>% filter( (population >= input$pop_range[1] ) &
                        (population <= input$pop_range[2] ))  %>% 
        ungroup()
      
    } else if (input$level_choice == "District") {
      data <- base_data %>%  filter(   (continent %in%  input$continent_aq ) &
                                         (region    %in%  input$region_aq    ) &
                                         (country   %in%  input$country_aq   ) &
                                         (name_1    %in%  input$state_aq     ) &
                                         (name_2    %in%  input$district_aq  )) %>% 
        
        group_by(country, name_1, name_2, year, category) %>%
        summarise(
          tot_pop = sum(population, na.rm = TRUE),
          # PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
          .groups = "drop"
        ) %>%  group_by(country, name_1, name_2, year) %>%
        mutate(
          population = sum(tot_pop, na.rm = TRUE),
          pct = (tot_pop / population) * 100
        ) %>% filter( (population >= input$pop_range[1] ) &
                        (population <= input$pop_range[2] ))  %>% 
        ungroup()
    }
    
    # Filter by selected years
    data <- data %>% filter(year %in% input$year_aq)
    
    # X variable selector
    x_var <- switch(input$level_choice,
                    "Continent" = "continent",
                    "AQLI Region" = "region",
                    "Country" = "country",
                    "State" = "name_1",
                    "District" = "name_2"
    )
    
    years <- sort(unique(data$year))
    
    # Reference ordering from latest year
    ref_year <- max(years)
    category_order <- data %>%
      filter(year == ref_year) %>%
      group_by(!!sym(x_var)) %>%
      summarise(tot_pop = sum(tot_pop), .groups = "drop") %>%
      arrange(tot_pop) %>%
      pull(!!sym(x_var))
    
    # -------------------------------
    # ⚡ Number formatting JS function
    # -------------------------------
    js_format <- JS("
function() {
    let v = this.value;
    if (v >= 1e9) return (v/1e9).toFixed(2) + 'B';
    if (v >= 1e6) return (v/1e6).toFixed(2) + 'M';
    if (v >= 1e3) return (v/1e3).toFixed(2) + 'K';
    return v;
}
")
    
    js_datalabel <- JS("
function() {
    let v = this.y;
    if (v >= 1e9) return (v/1e9).toFixed(2) + 'B';
    if (v >= 1e6) return (v/1e6).toFixed(2) + 'M';
    if (v >= 1e3) return (v/1e3).toFixed(2) + 'K';
    return v;
}
")
    
    js_tooltip <- JS("
                  function() {
                      let v = this.y;
                      if (v >= 1e9) v = (v/1e9).toFixed(2) + 'B';
                      else if (v >= 1e6) v = (v/1e6).toFixed(2) + 'M';
                      else if (v >= 1e3) v = (v/1e3).toFixed(2) + 'K';
                      return '<b>' + this.series.stackKey.replace('stack','') +
                             '</b><br>' +
                             '<span style=\"color:' + this.color + '\">●</span> ' +
                             this.series.name + ': <b>' + v + '</b>';
                  }
                  ")
    
    # -------------------------------
    # ⚡ Base Highchart
    # -------------------------------
    hc <- highchart() %>%
      hc_chart(type = "column",    event= list(
        selection = JS_Zoom
      ),
      zoomType= "x", style = list(fontFamily = "Lato, sans-serif")) %>%
      hc_title(
        text = title_all,
        align = "center",
        style = list(color = "black", fontSize = "18px", fontWeight = "bold")
      ) %>%
      hc_xAxis(
        categories = category_order,
        title = list(
          text = input$level_choice,
          style = list(color = "#333333", fontSize = "13px")
        ),
        labels = list(style = list(color = "#4A4A4A", fontSize = "12px")),
        lineColor = "#CCCCCC",
        tickColor = "#CCCCCC"
      ) %>%
      hc_yAxis(
        # type = "logarithmic",
        title = list(
          text = "Population Percentage(%)",
          style = list(color = "#333333", fontSize = "13px")
        ),
        labels = list(
          formatter = js_format,
          style = list(color = "#4A4A4A", fontSize = "12px")
        ),
        minorTickInterval = 0.1,
        gridLineColor = "#E0E0E0"
      ) %>%
      hc_tooltip(
        shared = FALSE,
        useHTML = TRUE,
        formatter = js_tooltip,
        backgroundColor = "rgba(255,255,255,0.95)",
        borderColor = "#CCCCCC",
        style = list(fontSize = "12px")
      ) %>%
      hc_legend(
        enabled = TRUE,
        align = "center",
        verticalAlign = "bottom",
        itemStyle = list(color = "#416891", fontWeight = "normal"),
        itemHoverStyle = list(color = "#ffa521")
      ) %>%
      
      hc_plotOptions(
        column = list(
          stacking = "normal",
          borderWidth = 0,
          pointPadding = 0.05,
          groupPadding = 0.05
        ),
        series = list(
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("
    function() {
      const n = this.y;
      if (n >= 1e9) {
        return (n/1e9).toFixed(1) + 'B';
      } else if (n >= 1e6) {
        return (n/1e6).toFixed(1) + 'M';
      } else if (n >= 1e3) {
        return (n/1e3).toFixed(1) + 'K';
      } else {
        return n.toFixed(1);
      }
    }
  "),
            style = list(
              color = "black",        # ← label color black
              textOutline = "none",
              fontSize = "11px",
              fontWeight = "bold"
            )
          )
        )
      ) %>%
      hc_colors(c("#69B3A2", "#F26B38")) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_credits(
        enabled = TRUE,
        text = "Air Quality Life Index (AQLI)",
        href = "https://aqli.epic.uchicago.edu",
        style = list(color = "#7D8CA3", fontSize = "11px")
      )
    
    # ---------------------------------
    # ⚡ Add series for each year
    # Stacked inside year, grouped by year
    # ---------------------------------
    for (yr in years) {
      
      # BELOW WHO
      df_below <- data %>%
        filter(year == yr, category == "Below Nat") %>%
        right_join(tibble(!!sym(x_var) := category_order), by = x_var) %>%
        arrange(factor(!!sym(x_var), levels = category_order)) %>%
        mutate(pct = replace_na(pct, 0)) %>%
        pull(pct)
      
      hc <- hc %>%
        hc_add_series(
          name = "Below Nat",
          data = df_below,
          stack = paste0("stack", yr),
          showInLegend = (yr == years[1]),
          color = "#69B3A2"
        )
      
      # ABOVE WHO
      df_above <- data %>%
        filter(year == yr, category == "Above Nat") %>%
        right_join(tibble(!!sym(x_var) := category_order), by = x_var) %>%
        arrange(factor(!!sym(x_var), levels = category_order)) %>%
        mutate(pct = replace_na(pct, 0)) %>%
        pull(pct)
      
      hc <- hc %>%
        hc_add_series(
          name = "Above Nat",
          data = df_above,
          stack = paste0("stack", yr),
          showInLegend = (yr == years[1]),
          color = "#F26B38"
        )
    }
    
    hc
    
    
  }else {
  
  y_axis <- "PM2.5 (µg/m³)"
  title_all <- paste0("<span style='color:maroon;'>",
                      input$level_choice,
                      " Wise Population Exposure to PM2.5</span>")  # Updated to match plot
  
  # Common select: include Region for all cases, only PM columns
  print("---------------------------------------------------------")
  
  # print(gadm2_data_filter())
  base_select <- gadm2_aqli  %>%  select(name_2, population, continent, country, name_1, region, natstandard, starts_with("pm")) 

  
  # Pivot PM only
  base_data <- base_select %>%
    pivot_longer(
      cols = starts_with("pm"),
      names_to = "year_str",
      names_prefix = "pm",
      values_to = "PM2.5"
    ) %>%
    mutate(
      year = as.numeric(year_str),
      category = ifelse(PM2.5 > natstandard, "Above Nat", "Below Nat")
    ) %>%
    filter(!is.na(category)) 
  
  # Now group and summarise based on level_choice
  if (input$level_choice == "Continent") {
    data <- base_data %>% filter( (continent  %in%  input$continent_aq ) ) %>% 
      group_by(continent, year, category) %>%
      summarise(
        tot_pop = sum(population, na.rm = TRUE),
        #PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
        .groups = "drop"
      ) %>%  group_by(continent, year) %>%
      mutate(
        population = sum(tot_pop, na.rm = TRUE),
        pct = (tot_pop / population) * 100
      ) %>% filter( (population >= input$pop_range[1] ) &
                      (population <= input$pop_range[2] ))  %>% 
      ungroup()
    
  } else if (input$level_choice == "AQLI Region") {
    data <- base_data %>%  filter(   (continent %in%  input$continent_aq ) &
                                       (region    %in%  input$region_aq    )) %>% 
      group_by(region, year, category) %>%
      summarise(
        tot_pop = sum(population, na.rm = TRUE),
        #PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
        .groups = "drop"
      ) %>%  group_by(region, year) %>%
      mutate(
        population = sum(tot_pop, na.rm = TRUE),
        pct = (tot_pop / population) * 100
      ) %>% filter( (population >= input$pop_range[1] ) &
                      (population <= input$pop_range[2] ))  %>% 
      ungroup()
    
  } else if (input$level_choice == "Country") {
    data <- base_data %>%   filter(   (continent %in%  input$continent_aq ) &
                                        (region    %in%  input$region_aq    ) &
                                        (country   %in%  input$country_aq   ) ) %>% 
      group_by(country, year, category) %>%
      summarise(
        tot_pop = sum(population, na.rm = TRUE),
       # PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
        .groups = "drop"
      ) %>% group_by(country, year) %>%
      mutate(
        population = sum(tot_pop, na.rm = TRUE),
        pct = (tot_pop / population) * 100
      ) %>% filter( (population >= input$pop_range[1] ) &
                      (population <= input$pop_range[2] ))  %>% 
      ungroup()
    
  } else if (input$level_choice == "State") {
    data <- base_data %>%   filter(   (continent %in%  input$continent_aq ) &
                                        (region    %in%  input$region_aq    ) &
                                        (country   %in%  input$country_aq   ) &
                                        (name_1    %in%  input$state_aq     ) ) %>% 
      group_by(country, name_1, year, category) %>%
      summarise(
        tot_pop = sum(population, na.rm = TRUE),
       # PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
        .groups = "drop"
      ) %>%  group_by(country, name_1, year) %>%
      mutate(
        population = sum(tot_pop, na.rm = TRUE),
        pct = (tot_pop / population) * 100
      ) %>% filter( (population >= input$pop_range[1] ) &
                      (population <= input$pop_range[2] ))  %>% 
      ungroup()
    
  } else if (input$level_choice == "District") {
    data <- base_data %>%  filter(   (continent %in%  input$continent_aq ) &
                                       (region    %in%  input$region_aq    ) &
                                       (country   %in%  input$country_aq   ) &
                                       (name_1    %in%  input$state_aq     ) &
                                       (name_2    %in%  input$district_aq  )) %>% 
      
      group_by(country, name_1, name_2, year, category) %>%
      summarise(
        tot_pop = sum(population, na.rm = TRUE),
       # PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE),
        .groups = "drop"
      ) %>%  group_by(country, name_1, name_2, year) %>%
      mutate(
        population = sum(tot_pop, na.rm = TRUE),
        pct = (tot_pop / population) * 100
      ) %>% filter( (population >= input$pop_range[1] ) &
                      (population <= input$pop_range[2] ))  %>% 
      ungroup()
  }
  
  # Filter by selected years
  data <- data %>% filter(year %in% input$year_aq)
  
  # X variable selector
  x_var <- switch(input$level_choice,
                  "Continent" = "continent",
                  "AQLI Region" = "region",
                  "Country" = "country",
                  "State" = "name_1",
                  "District" = "name_2"
  )
  
  years <- sort(unique(data$year))
  
  # Reference ordering from latest year
  ref_year <- max(years)
  category_order <- data %>%
    filter(year == ref_year) %>%
    group_by(!!sym(x_var)) %>%
    summarise(tot_pop = sum(tot_pop), .groups = "drop") %>%
    arrange(tot_pop) %>%
    pull(!!sym(x_var))
  
  # -------------------------------
  # ⚡ Number formatting JS function
  # -------------------------------
  js_format <- JS("
function() {
    let v = this.value;
    if (v >= 1e9) return (v/1e9).toFixed(2) + 'B';
    if (v >= 1e6) return (v/1e6).toFixed(2) + 'M';
    if (v >= 1e3) return (v/1e3).toFixed(2) + 'K';
    return v;
}
")
  
  js_datalabel <- JS("
function() {
    let v = this.y;
    if (v >= 1e9) return (v/1e9).toFixed(2) + 'B';
    if (v >= 1e6) return (v/1e6).toFixed(2) + 'M';
    if (v >= 1e3) return (v/1e3).toFixed(2) + 'K';
    return v;
}
")
  
  js_tooltip <- JS("
                  function() {
                      let v = this.y;
                      if (v >= 1e9) v = (v/1e9).toFixed(2) + 'B';
                      else if (v >= 1e6) v = (v/1e6).toFixed(2) + 'M';
                      else if (v >= 1e3) v = (v/1e3).toFixed(2) + 'K';
                      return '<b>' + this.series.stackKey.replace('stack','') +
                             '</b><br>' +
                             '<span style=\"color:' + this.color + '\">●</span> ' +
                             this.series.name + ': <b>' + v + '</b>';
                  }
                  ")
  
  # -------------------------------
  # ⚡ Base Highchart
  # -------------------------------
  hc <- highchart() %>%
    hc_chart(type = "column",    event= list(
      selection = JS_Zoom
    ),
    zoomType= "x", style = list(fontFamily = "Lato, sans-serif")) %>%
    hc_title(
      text = title_all,
      align = "center",
      style = list(color = "black", fontSize = "18px", fontWeight = "bold")
    ) %>%
    hc_xAxis(
      categories = category_order,
      title = list(
        text = input$level_choice,
        style = list(color = "#333333", fontSize = "13px")
      ),
      labels = list(style = list(color = "#4A4A4A", fontSize = "12px")),
      lineColor = "#CCCCCC",
      tickColor = "#CCCCCC"
    ) %>%
    hc_yAxis(
      # type = "logarithmic",
      title = list(
        text = "Population",
        style = list(color = "#333333", fontSize = "13px")
      ),
      labels = list(
        formatter = js_format,
        style = list(color = "#4A4A4A", fontSize = "12px")
      ),
      minorTickInterval = 0.1,
      gridLineColor = "#E0E0E0"
    ) %>%
    hc_tooltip(
      shared = FALSE,
      useHTML = TRUE,
      formatter = js_tooltip,
      backgroundColor = "rgba(255,255,255,0.95)",
      borderColor = "#CCCCCC",
      style = list(fontSize = "12px")
    ) %>%
    hc_legend(
      enabled = TRUE,
      align = "center",
      verticalAlign = "bottom",
      itemStyle = list(color = "#416891", fontWeight = "normal"),
      itemHoverStyle = list(color = "#ffa521")
    ) %>%
    hc_plotOptions(
      column = list(
        stacking = "normal",
        borderWidth = 0,
        pointPadding = 0.05,
        groupPadding = 0.05
      ),
      series = list(
        dataLabels = list(
          enabled = TRUE,
          formatter = JS("
    function() {
      const n = this.y;
      if (n >= 1e9) {
        return (n/1e9).toFixed(1) + 'B';
      } else if (n >= 1e6) {
        return (n/1e6).toFixed(1) + 'M';
      } else if (n >= 1e3) {
        return (n/1e3).toFixed(1) + 'K';
      } else {
        return n.toFixed(1);
      }
    }
  "),
          style = list(
            color = "black",        # ← label color black
            textOutline = "none",
            fontSize = "11px",
            fontWeight = "bold"
          )
        )
      )
    ) %>%
    hc_colors(c("#69B3A2", "#F26B38")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_credits(
      enabled = TRUE,
      text = "Air Quality Life Index (AQLI)",
      href = "https://aqli.epic.uchicago.edu",
      style = list(color = "#7D8CA3", fontSize = "11px")
    )
  
  # ---------------------------------
  # ⚡ Add series for each year
  # Stacked inside year, grouped by year
  # ---------------------------------
  for (yr in years) {
    
    # BELOW WHO
    df_below <- data %>%
      filter(year == yr, category == "Below Nat") %>%
      right_join(tibble(!!sym(x_var) := category_order), by = x_var) %>%
      arrange(factor(!!sym(x_var), levels = category_order)) %>%
      mutate(tot_pop = replace_na(tot_pop, 0)) %>%
      pull(tot_pop)
    
    hc <- hc %>%
      hc_add_series(
        name = "Below Nat",
        data = df_below,
        stack = paste0("stack", yr),
        showInLegend = (yr == years[1]),
        color = "#69B3A2"
      )
    
    # ABOVE WHO
    df_above <- data %>%
      filter(year == yr, category == "Above Nat") %>%
      right_join(tibble(!!sym(x_var) := category_order), by = x_var) %>%
      arrange(factor(!!sym(x_var), levels = category_order)) %>%
      mutate(tot_pop = replace_na(tot_pop, 0)) %>%
      pull(tot_pop)
    
    hc <- hc %>%
      hc_add_series(
        name = "Above Nat",
        data = df_above,
        stack = paste0("stack", yr),
        showInLegend = (yr == years[1]),
        color = "#F26B38"
      )
  }
  
  hc
  
  
  }
  
  # hc <- highchart() %>%
  #   hc_chart(
  #     type = "column",
  #     style = list(fontFamily = "Lato, sans-serif")
  #   ) %>%
  #   hc_title(
  #     text = title_all,
  #     align = "center",
  #     style = list(
  #       color = "black",
  #       fontSize = "18px",
  #       fontWeight = "bold"
  #     )
  #   ) %>%
  #   hc_xAxis(
  #     categories = category_order,
  #     title = list(
  #       text = input$level_choice,
  #       style = list(color = "#333333", fontSize = "13px")
  #     ),
  #     labels = list(
  #       style = list(color = "#4A4A4A", fontSize = "12px")
  #       # rotation = -45  # Uncomment for District if labels overlap
  #     ),
  #     lineColor = "#CCCCCC",
  #     tickColor = "#CCCCCC"
  #   ) %>%
  #   hc_yAxis(
  #     title = list(
  #       text = y_axis,
  #       style = list(color = "#333333", fontSize = "13px")
  #     ),
  #     gridLineColor = "#E0E0E0",
  #     labels = list(style = list(color = "#4A4A4A", fontSize = "12px"))
  #   ) %>%
  #   hc_tooltip(
  #     shared = TRUE,
  #     backgroundColor = "rgba(255, 255, 255, 0.95)",
  #     borderColor = "#CCCCCC",
  #     style = list(fontSize = "12px"),
  #     headerFormat = '<b style="font-size:13px;">{point.key}</b><br>',
  #     pointFormat = '<span style="color:{series.color}">\u25CF</span> {series.name}: <b>{point.y:.1f}</b><br>'
  #   ) %>%
  #   hc_legend(
  #     enabled = TRUE,
  #     align = "center",
  #     verticalAlign = "bottom",
  #     itemStyle = list(color = "#416891", fontWeight = "normal"),
  #     itemHoverStyle = list(color = "#ffa521")
  #   ) %>%
  #   hc_plotOptions(
  #     column = list(
  #       grouping = TRUE,  # Groups bars for categories side-by-side
  #       shadow = FALSE,
  #       borderWidth = 0,
  #       pointPadding = 0.1,
  #       groupPadding = 0.05,
  #       dataLabels = list(
  #         enabled = TRUE,
  #         format = "{y:.1f}",
  #         style = list(color = "#4A4A4A", fontSize = "11px")
  #       )
  #     )
  #   ) %>%
  #   hc_colors(c("#416891", "#F26B38", "#69B3A2", "#FFB81C", "#7D8CA3")) %>% # AQLI palette
  #   hc_exporting(enabled = TRUE) %>%
  #   hc_credits(
  #     enabled = TRUE,
  #     text = "Air Quality Life Index (AQLI)",
  #     href = "https://aqli.epic.uchicago.edu",
  #     style = list(color = "#7D8CA3", fontSize = "11px")
  #   )
  # 
  # # Add series for each year and category
  # for (yr in years) {
  #   for (cat in c("Below WHO", "Above WHO")) {
  #     df_cat <- data %>%
  #       filter(year == yr, category == cat) %>%
  #       right_join(tibble(!!sym(x_var) := category_order), by = x_var) %>%
  #       arrange(factor(!!sym(x_var), levels = category_order)) %>%
  #       mutate(PM2.5_avg = replace_na(PM2.5_avg, 0)) %>%
  #       pull(PM2.5_avg)
  #     
  #     # Fixed colors: green-ish for below, orange-red for above
  #     col_choice <- if (cat == "Below WHO") "#69B3A2" else "#F26B38"
  #     
  #     hc <- hc %>%
  #       hc_add_series(
  #         name = paste(yr, cat),
  #         data = df_cat,
  #         color = col_choice,
  #         tooltip = list(valueSuffix = " µg/m³")
  #       )
  #   }
  # }
  # 
  # hc
})

# output$pop_below_above_who <- renderHighchart({
# 
# 
#     #colors <- c("#FF8C00", "#1F4E79", "#FFB84D", "#336699", "#FF9933") # dark orange & blue shades
# 
#   y_axis = "PM2.5 (µg/m³)"
#   title_all <- paste0( "<span style='color:maroon;'>",
#                        input$level_choice,
#                        " Wise Life Year Loss Per Person (Nat.)")
# 
#     if(input$level_choice == "Continent"){
# 
# 
# data <- gadm2_aqli %>% select(name_2, population, continent, country, name_1, starts_with("pm")) %>%
#   pivot_longer(
#     starts_with("pm"), names_to = "year", names_prefix = "pm", values_to = "PM2.5") %>%
#   mutate(
#     year = as.numeric(year),
#     category = ifelse(PM2.5 > who_guideline, "Above WHO", "Below WHO")
#   ) %>%
# group_by(continent, year, category) %>%
#     summarise(
#       tot_pop = sum(population, na.rm = TRUE),
#       .groups = "drop"
#     ) %>% filter(!is.na(category))
# 
# 
#     } else if(input$level_choice == "AQLI Region"){
# 
#       data <- gadm2_aqli %>% select(name_2, population, continent, country, name_1, starts_with("pm")) %>%
#         pivot_longer(
#           starts_with("pm"), names_to = "year", names_prefix = "pm", values_to = "PM2.5") %>%
#         mutate(
#           year = as.numeric(year),
#           category = ifelse(PM2.5 > who_guideline, "Above WHO", "Below WHO")
#         ) %>%
#         group_by(Region, year, category) %>%
#         summarise(
#           tot_pop = sum(population, na.rm = TRUE),
#           .groups = "drop"
#         ) %>% filter(!is.na(category))
# 
#     } else if(input$level_choice == "Country"){
# 
#       data <- gadm2_aqli %>% select(name_2, population, continent, country, name_1, starts_with("pm")) %>%
#         pivot_longer(
#           starts_with("pm"), names_to = "year", names_prefix = "pm", values_to = "PM2.5") %>%
#         mutate(
#           year = as.numeric(year),
#           category = ifelse(PM2.5 > who_guideline, "Above WHO", "Below WHO")
#         ) %>%
#         group_by(country, year, category) %>%
#         summarise(
#           tot_pop = sum(population, na.rm = TRUE),
#           .groups = "drop"
#         ) %>% filter(!is.na(category))
# 
# 
#     } else if(input$level_choice == "State"){
# 
#       data <- gadm2_aqli %>% select(name_2, population, continent, country, name_1, starts_with("pm")) %>%
#         pivot_longer(
#           starts_with("pm"), names_to = "year", names_prefix = "pm", values_to = "PM2.5") %>%
#         mutate(
#           year = as.numeric(year),
#           category = ifelse(PM2.5 > who_guideline, "Above WHO", "Below WHO")
#         ) %>%
#         group_by(country, name_1, year, category) %>%
#         summarise(
#           tot_pop = sum(population, na.rm = TRUE),
#           .groups = "drop"
#         ) %>% filter(!is.na(category))
# 
# 
#     } else if(input$level_choice == "District"){
# 
#       data <- gadm2_aqli %>% select(name_2, population, continent, country, name_1, starts_with("pm")) %>%
#         pivot_longer(
#           starts_with("pm"), names_to = "year", names_prefix = "pm", values_to = "PM2.5") %>%
#         mutate(
#           year = as.numeric(year),
#           category = ifelse(PM2.5 > who_guideline, "Above WHO", "Below WHO")
#         ) %>%
#         group_by(country, name_1, name_2, year, category) %>%
#         summarise(
#           tot_pop = sum(population, na.rm = TRUE),
#           .groups = "drop"
#         ) %>% filter(!is.na(category))
# 
#     }
# 
# 
#   data <- data %>% filter(year %in% input$year_aq)
# 
#   # X variable name
#   x_var <- switch(input$level_choice,
#                   "Continent" = "continent",
#                   "AQLI Region"    = "Region",
#                   "Country"   = "country",
#                   "State"     = "name_1",
#                   "District"  = "name_2"
#   )
# 
#   years <- sort(unique(data$year))
# 
#   # ---- Choose reference order (latest year) ----
#   ref_year <- max(years)
# 
#   category_order <- data %>%
#     filter(year == ref_year) %>%
#     arrange(tot_pop) %>%          # increasing order of PM2.5
#     pull(!!sym(x_var))
# 
# 
# 
#   # AQLI Themed Highchart: Multi-Year Column Chart
#   hc <- highchart() %>%
#     hc_chart(
#       type = "column",
#       # backgroundColor = "#FFFFFF",
#       style = list(fontFamily = "Lato, sans-serif")
#     ) %>%
#     hc_title(
#       text = title_all,
#       align = "center",
#       style = list(
#         color = "black",   # AQLI Dark Blue
#         fontSize = "18px",
#         fontWeight = "bold"
#       )
#     ) %>%
#     hc_xAxis(
#       categories = category_order,
#       title = list(
#         text = input$level_choice,
#         style = list(color = "#333333", fontSize = "13px")
#       ),
#       labels = list(
#         style = list(color = "#4A4A4A", fontSize = "12px")
#         # rotation = -45
#       ),
#       lineColor = "#CCCCCC",
#       tickColor = "#CCCCCC"
#     ) %>%
#     hc_yAxis(
#       title = list(
#         text = y_axis,
#         style = list(color = "#333333", fontSize = "13px")
#       ),
#       gridLineColor = "#E0E0E0",
#       labels = list(style = list(color = "#4A4A4A", fontSize = "12px"))
#     ) %>%
#     hc_tooltip(
#       shared = TRUE,
#       backgroundColor = "rgba(255, 255, 255, 0.95)",
#       borderColor = "#CCCCCC",
#       style = list(fontSize = "12px"),
#       headerFormat = '<b style="font-size:13px;">{point.key}</b><br>',
#       pointFormat = '<span style="color:{series.color}">\u25CF</span> {series.name}: <b>{point.y:.1f}</b><br>'
#     ) %>%
#     hc_legend(
#       enabled = TRUE,
#       align = "center",
#       verticalAlign = "bottom",
#       itemStyle = list(color = "#416891", fontWeight = "normal"),
#       itemHoverStyle = list(color = "#ffa521")
#     ) %>%
#     hc_plotOptions(
#       column = list(
#         grouping = TRUE,
#         shadow = FALSE,
#         borderWidth = 0,
#         pointPadding = 0.1,
#         dataLabels = list(
#           enabled = TRUE,
#           format = "{y:.1f}",
#           style = list(color = "#4A4A4A", fontSize = "11px")
#         )
#       )
#     ) %>%
#     hc_colors(c("#416891", "#F26B38", "#69B3A2", "#FFB81C", "#7D8CA3")) %>%  # AQLI palette
#     hc_exporting(enabled = TRUE) %>%
#     hc_credits(
#       enabled = TRUE,
#       text = "Air Quality Life Index (AQLI)",
#       href = "https://aqli.epic.uchicago.edu",
#       style = list(color = "#7D8CA3", fontSize = "11px")
#     )
# 
#   # Add series for each year in loop
#   for (yr in years) {
#     df_year <- data %>%
#       filter(year == yr) %>%
#       right_join(tibble(!!sym(x_var) := category_order), by = x_var) %>%
#       arrange(factor(!!sym(x_var), levels = category_order))
# 
#     hc <- hc %>%
#       hc_add_series(
#         name = as.character(yr),
#         data = df_year$PM2.5,
#         tooltip = list(valueSuffix = " µg/m³")
#       )
#   }
# 
#   hc
# })



# End Here


# Total LLPP Calculation (Uncomment for Visual)

# output$bar_aqli_comp_llwp <- renderHighchart({
#   
#   if (input$aqli_comp_choice == "pm_comp_25") {
# 
#   #   
#     colors <- c("#FF8C00", "#1F4E79", "#FFB84D", "#336699", "#FF9933") # dark orange & blue shades
#     title_all <- paste("Total Life Year Loss (WHO) by", input$level_choice, "and Year")
#     y_axis = "Year (millions)"
#     
#     if(input$level_choice == "Continent"){
# 
#       data = continent_weighted %>%
#         select("continent", "population", all_of(starts_with("pm"))) %>%
#         setNames(c("continent", "population", as.character(1998:2024))) %>%
#         pivot_longer(!c(continent,population),names_to = "year", values_to = "PM2.5") %>% mutate(
#           tot_lyl = population*PM2.5)
# 
# 
#     } else if(input$level_choice == "Country"){
# 
#       data = gadm_df_aqli_country() %>%
#         select("country","continent", all_of(starts_with("pm"))) %>%
#         setNames(c("country", "continent",  as.character(1998:2024))) %>%# select(-c("population", "whostandard", "natstandard"))
#         pivot_longer(!c(country, continent), names_to = "year", values_to = "PM2.5") %>% mutate(
#           tot_lyl = population*PM2.5)
# 
# 
#     } else if(input$level_choice == "State"){
# 
#       data = gadm_df_aqli_state() %>%
#         select("name_1","continent", "country",all_of(starts_with("pm"))) %>%
#         setNames(c( "name_1", "continent", "country",as.character(1998:2024))) %>%
#         pivot_longer(!c(continent, country, name_1),names_to = "year", values_to = "PM2.5") %>% mutate(
#           tot_lyl = population*PM2.5)
# 
# 
#     } else if(input$level_choice == "District"){
# 
#       data = gadm_df_aqli_district() %>%
#         select("name_2", "continent", "country", "name_1",  all_of(starts_with("pm"))) %>%
#         setNames(c("name_2", "continent", "country", "name_1",  as.character(1998:2024))) %>%
#         pivot_longer(!c(name_2,continent,country, name_1), names_to = "year", values_to = "PM2.5") %>% mutate(
#           tot_lyl = population*PM2.5)
# 
# 
#     }
#   #   
#   } else if(input$aqli_comp_choice == "llpp_comp"){
#       
#     colors <- c("#FF8C00", "#1F4E79", "#FFB84D", "#336699", "#FF9933") # dark orange & blue shades
#     title_all <- paste("Total Life Year Loss (WHO) by", input$level_choice, "and Year")
#     y_axis = "Year (millions)"
#     
#     
#     if(input$level_choice == "Continent"){
#       
#       data = gadm_df_aqli_continent() %>%
#         select("continent", "population", all_of(starts_with("llpp_who"))) %>%
#         setNames(c("continent", "population", as.character(1998:2024))) %>%
#         pivot_longer(!c(continent, population), names_to = "year", values_to = "PM2.5") %>% mutate(
#           tot_lyl = population*PM2.5)
#       
#     } else if(input$level_choice == "Country"){
#       
#       data = gadm_df_aqli_country() %>%
#         select("country","continent", "population", all_of(starts_with("llpp_who"))) %>%
#         setNames(c("country", "continent", "population",  as.character(1998:2024))) %>%# select(-c("population", "whostandard", "natstandard"))
#         pivot_longer(!c(country,continent,population), names_to = "year", values_to = "PM2.5") %>% mutate(
#           tot_lyl = population*PM2.5)
#       
#       
#     } else if(input$level_choice == "State"){
#       
#       data = gadm_df_aqli_state() %>%
#         select("name_1","continent", "country", "population", all_of(starts_with("llpp_who"))) %>%
#         setNames(c( "name_1", "continent", "country","population", as.character(1998:2024))) %>%
#         pivot_longer(!c(continent, country, name_1, population),names_to = "year", values_to = "PM2.5") %>% mutate(
#           tot_lyl = population*PM2.5)
#       
#       
#     } else if(input$level_choice == "District"){
#       
#       data = gadm_df_aqli_district() %>%
#         select("name_2", "continent", "country", "name_1", "population", all_of(starts_with("llpp_who"))) %>%
#         setNames(c("name_2", "continent", "country", "name_1", "population",  as.character(1998:2024))) %>%
#         pivot_longer(!c(name_2,continent,country, name_1, population), names_to = "year", values_to = "PM2.5") %>% mutate(
#           tot_lyl = population*PM2.5)
#       
#       
#     } 
#     
#     
#     
#   } else if(input$aqli_comp_choice == "nat_llpp_comp"){
#     colors <- c("#FF8C00", "#1F4E79", "#FFB84D", "#336699", "#FF9933") # dark orange & blue shades
#     title_all <- paste("Total Life Year Loss (Nat) by", input$level_choice, "and Year")
#     y_axis = "Year (millions)"
#     
#     if(input$level_choice == "Continent"){
#       
#       data = gadm_df_aqli_continent() %>%
#         select("continent", "population", all_of(starts_with("llpp_nat"))) %>%
#         setNames(c("continent", "population", as.character(1998:2024))) %>%
#         pivot_longer(!c(continent, population),names_to = "year", values_to = "PM2.5") %>% mutate(
#           tot_lyl = population*PM2.5)
#       
#     } else if(input$level_choice == "Country"){
#       
#       data = gadm_df_aqli_country() %>%
#         select("country","continent", "population", all_of(starts_with("llpp_nat"))) %>%
#         setNames(c("country", "continent", "population",  as.character(1998:2024))) %>%# select(-c("population", "whostandard", "natstandard"))
#         pivot_longer(!c(country, continent, population), names_to = "year", values_to = "PM2.5") %>% mutate(
#           tot_lyl = population*PM2.5)
#       
#       
#     } else if(input$level_choice == "State"){
#       
#       data = gadm_df_aqli_state() %>%
#         select("name_1","continent", "country", "population", all_of(starts_with("llpp_nat"))) %>%
#         setNames(c( "name_1", "continent", "country","population", as.character(1998:2024))) %>%
#         pivot_longer(!c(continent, country, name_1, population), names_to = "year", values_to = "PM2.5") %>% mutate(
#           tot_lyl = population*PM2.5)
#       
#       
#     } else if(input$level_choice == "District"){
#       
#       data = gadm_df_aqli_district() %>%
#         select("name_2", "continent", "country", "name_1",  all_of(starts_with("llpp_nat"))) %>%
#         setNames(c("name_2", "continent", "country", "name_1", "population",  as.character(1998:2024))) %>%
#         pivot_longer(!c(name_2,continent, country, name_1, population), names_to = "year", values_to = "PM2.5") %>% mutate(
#           tot_lyl = population*PM2.5)
#       
#       
#     } 
#     
#     
#   }
#   
#   data <- data %>% filter(year %in% input$year_aq)
#   
#   x_var <- switch(input$level_choice,
#                   "Continent" = "continent",
#                   "Country"   = "country",
#                   "State"     = "name_1",
#                   "District"  = "name_2" )
#   
#   years <- sort(unique(data$year))
#   
#   # ---- Choose reference order (latest year) ----
#   ref_year <- max(years)
#   
#   category_order <- data %>%
#     filter(year == ref_year) %>%
#     arrange(tot_lyl) %>%          # increasing order of tot_lyl
#     pull(!!sym(x_var))
#   
#   # Highchart setup
#   # AQLI Themed Highchart: Multi-Year Horizontal Bar Chart
#   hc <- highchart() %>%
#     hc_chart(
#       type = "bar",
#      # backgroundColor = "#FFFFFF",
#       style = list(fontFamily = "Lato, sans-serif")
#     ) %>%
#     hc_title(
#       text = title_all,
#       align = "center",
#       style = list(
#         color = "#002D72",   # AQLI Dark Blue
#         fontSize = "18px",
#         fontWeight = "bold"
#       )
#     ) %>%
#     hc_xAxis(
#       categories = category_order,
#       title = list(
#         text = input$level_choice,
#         style = list(color = "#333333", fontSize = "13px")
#       ),
#       labels = list(
#         style = list(color = "#4A4A4A", fontSize = "12px")
#       ),
#       lineColor = "#CCCCCC",
#       tickColor = "#CCCCCC"
#     ) %>%
#     hc_yAxis(
#       title = list(
#         text = y_axis,
#         style = list(color = "#333333", fontSize = "13px")
#       ),
#       gridLineColor = "#E0E0E0",
#       labels = list(style = list(color = "#4A4A4A", fontSize = "12px"))
#     ) %>%
#     hc_tooltip(
#       shared = TRUE,
#       backgroundColor = "rgba(255, 255, 255, 0.95)",
#       borderColor = "#CCCCCC",
#       style = list(fontSize = "12px"),
#       headerFormat = '<b style="font-size:13px;">{point.key}</b><br>',
#       pointFormat = '<span style="color:{series.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}</b><br>'
#     ) %>%
#     hc_legend(
#       enabled = TRUE,
#       align = "center",
#       verticalAlign = "bottom",
#       itemStyle = list(color = "#002D72", fontWeight = "normal"),
#       itemHoverStyle = list(color = "#F26B38")
#     ) %>%
#     hc_plotOptions(
#       bar = list(
#         grouping = TRUE,
#         shadow = FALSE,
#         borderWidth = 0,
#         pointPadding = 0.15,
#         dataLabels = list(
#           enabled = TRUE,
#           format = "{y:.2f}",
#           style = list(color = "#4A4A4A", fontSize = "11px")
#         )
#       )
#     ) %>%
#     hc_colors(c("#002D72", "#F26B38", "#69B3A2", "#FFB81C", "#7D8CA3")) %>%
#     hc_exporting(enabled = TRUE) %>%
#     hc_credits(
#       enabled = TRUE,
#       text = "Air Quality Life Index (AQLI)",
#       href = "https://aqli.epic.uchicago.edu",
#       style = list(color = "#7D8CA3", fontSize = "11px")
#     )
#   
#   # Add data series for each year
#   for (yr in years) {
#     df_year <- data %>%
#       filter(year == yr) %>%
#       right_join(tibble(!!sym(x_var) := category_order), by = x_var) %>%
#       arrange(factor(!!sym(x_var), levels = category_order))
#     
#     hc <- hc %>%
#       hc_add_series(
#         name = as.character(yr),
#         data = round(df_year$tot_lyl/1000000,2),
#         tooltip = list(valueSuffix = " years")
#       )
#   }
#   
#   hc
# })

#### Important ggplot chart
# output$region_with_below_and_above <- renderPlot({
#  
#   # if (input$aqli_comp_choice == "pm_comp_25") {
#     
#     colors <- c("#FF8A3D", "#416891") # orange = above WHO, blue = below WHO
#    # title_all <- paste("PM2.5 compliance by", input$level_choice, "and Year")
#     
#     title_all = paste(
#       "PM<sub>2.5</sub> compliance by <span style='color:maroon;'>", input$level_choice, 
#       "</span> and Year"
#     )
#     
#     y_axis <- "Number of Regions"
#     
#     # ----- Select data depending on level -----
#     if (input$level_choice == "Continent") {
#       data <- gadm0_aqli %>% filter(continent %in% input$continent_aq) %>%
#         select(country, continent, starts_with("pm")) %>%
#         setNames(c("country", "continent", as.character(1998:2024))) %>%
#         pivot_longer(!c(country, continent), names_to = "year", values_to = "PM2.5")
#       
#     } else if (input$level_choice == "AQLI Region") {
#       data <- gadm0_aqli %>% filter(continent %in% input$continent_aq, Region %in% input$region_aq) %>%
#         select(country, continent, Region, starts_with("pm")) %>%
#         setNames(c("country", "continent", "Region", as.character(1998:2024))) %>%
#         pivot_longer(!c(country, continent, Region), names_to = "year", values_to = "PM2.5") %>% ungroup()
#       
#     } else if (input$level_choice == "Country") {
#       data <- gadm1_aqli_2023 %>% filter(continent %in% input$continent_aq, country %in% input$country_aq) %>%
#         select(country, continent, name_1, starts_with("pm")) %>%
#         setNames(c("country", "continent", "name_1", as.character(1998:2024))) %>%
#         pivot_longer(!c(country, continent, name_1), names_to = "year", values_to = "PM2.5") %>% ungroup()
#       
#     } else if (input$level_choice == "State") {
#       data <- gadm2_aqli %>% filter(continent %in% input$continent_aq, country %in% input$country_aq, name_1 %in% input$state_aq) %>%
#         select(country, continent, name_1,name_2, starts_with("pm")) %>%
#         setNames(c("country", "continent", "name_1", "name_2", as.character(1998:2024))) %>%
#         pivot_longer(!c(country, continent, name_1, name_2), names_to = "year", values_to = "PM2.5")
#       
#     } else if (input$level_choice == "District") {
#       data <- gadm2_aqli %>% filter(continent %in% input$continent_aq, country %in% input$country_aq, name_1 %in% input$state_aq) %>%
#         select(country, continent, name_1,name_2, starts_with("pm")) %>%
#         setNames(c("country", "continent", "name_1", "name_2", as.character(1998:2024))) %>%
#         pivot_longer(!c(country, continent, name_1, name_2), names_to = "year", values_to = "PM2.5")
#     }
#     
# 
#     
#     # ---- Group by region + year ----
#     x_var <- switch(input$level_choice,
#                     "Continent"    = "continent",
#                     "AQLI Region"  = "Region",
#                     "Country"      = "country",
#                     "State"        = "name_1",
#                     "District"     = "name_1"
#     )
#     
#     # --- Data preparation ---
#   data_prep <- data %>%
#   filter(year %in% input$year_aq) %>%
#   mutate(
#     below_who = ifelse(PM2.5 <= 5, 1, 0),
#     above_who = ifelse(PM2.5 > 5, 1, 0)
#   ) %>%
#   group_by(year, !!sym(x_var)) %>%
#   summarise(
#     `> WHO Standard` = sum(below_who, na.rm = TRUE),
#     `< WHO Standard` = sum(above_who, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   pivot_longer(cols = c(`> WHO Standard`,  `< WHO Standard`), 
#                names_to = "category", values_to = "count")
# 
#   data_prep <- data_prep %>% arrange(count)
# 
#   aqli_colors <- c(
#     `> WHO Standard` = "#416891",   # Deep Teal
#     `< WHO Standard` = "#FF8A3D"    # Modern Soft Orange
#   )
# 
#   # Prepare plot
#   ggplot(data_prep, aes(x = !!sym(x_var), y = count, fill = category)) +
#     geom_col(
#       position = "stack",
#       color = "white",  # keeps borders
#       #width = 0.55   # << reduce bar width here
#       width = 0.6
#       # width removed for auto-adjust
#     ) +
#     facet_wrap(~year, ncol = 1, scales = "free_y") +
#     scale_fill_manual(values = aqli_colors, name = "Category") +
#     labs(
#       title = "Regions Above and Below WHO Guideline",
#       x = input$level_choice,
#       y = "Count of Regions"
#     ) +
#     theme_minimal(base_size = 14) +
#     theme(
#       # Backgrounds
#       plot.background = element_rect(fill = "#F5F5F5", color = NA),    # light grey overall background
#       panel.background = element_rect(fill = "#F5F5F5", color = NA),   # light grey panel background
#       panel.grid.major.x = element_blank(),   # remove vertical grid
#       panel.grid.major.y = element_line(color = "#DCDCDC"),  # keep horizontal grid
#       panel.grid.minor = element_blank(),      # remove minor grids
# 
#       text = element_text(family = "Montserrat", color = "#2E2E2E"),
#       axis.text.x = element_text(face = NULL, size = 12, angle = 0, hjust = 0.5),
#       axis.text.y = element_text(size = 12),
#       axis.title = element_text(face = NULL, size = 14),
#       plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
#       strip.text = element_text(face = NULL, size = 13),
#       legend.position = "bottom",
#       legend.title = element_text(face = "bold", size = 12),
#       legend.text = element_text(size = 11)
#     ) +
#     geom_text(
#       aes(label = count),
#       position = position_stack(vjust = 0.5),
#       color = "white",
#       size = 4
#     #  fontface = "bold"
#     ) +
#     guides(fill = guide_legend(reverse = TRUE))
# 
#   
#   
# # --- Highcharter chart ---
# # hc <- highchart() %>%
# #   hc_chart(type = "column") %>%
# #   hc_title(text = title_all) %>%
# #   hc_xAxis(categories = unique(data_prep[[x_var]])) %>%
# #   hc_yAxis(title = list(text = "Number of Regions")) %>%
# #   hc_plotOptions(
# #     column = list(
# #       stacking = "normal",
# #       dataLabels = list(enabled = TRUE), # <-- label on each column segment
# #       groupPadding = 0.1
# #     )
# #   ) %>%
# #   hc_colors(c("#1F4E79", "#FF8C00")) # below=blue, above=orange
# # 
# # # --- Add series per year/category ---
# # for (cat in unique(data_prep$category)) {
# #   for (yr in unique(data_prep$year)) {
# #     series_data <- data_prep %>%
# #       filter(category == cat, year == yr) %>%
# #       pull(count)
# # 
# #     hc <- hc %>%
# #       hc_add_series(
# #         name = paste(cat, yr),
# #         data = series_data,
# #         stack = cat  # stack by category
# #       )
# #   }
# # }
# # 
# # hc
# #  }
#   
#   
#   ###################################
#   
#   # # Define AQLI theme with Montserrat
#   # hc_theme_aqli <- hc_theme(
#   #   chart = list(
#   #     backgroundColor = "#ffffff",
#   #     style = list(fontFamily = "Montserrat, Helvetica, Arial, sans-serif")
#   #   ),
#   #   colors = c("#E66101", "#5E3C99"), # dark orange + dark blue
#   #   xAxis = list(
#   #     gridLineWidth = 0,
#   #     lineColor = "#000000",
#   #     labels = list(style = list(color = "#000000", fontSize = "12px", fontFamily = "Montserrat"))
#   #   ),
#   #   yAxis = list(
#   #     gridLineColor = "#d9d9d9",
#   #     labels = list(style = list(color = "#000000", fontSize = "12px", fontFamily = "Montserrat")),
#   #     title = list(style = list(color = "#000000", fontSize = "14px", fontWeight = "bold", fontFamily = "Montserrat"))
#   #   ),
#   #   legend = list(
#   #     itemStyle = list(color = "#000000", fontWeight = "normal", fontSize = "12px", fontFamily = "Montserrat")
#   #   ),
#   #   tooltip = list(
#   #     backgroundColor = "#f0f0f0",
#   #     borderColor = "#000000",
#   #     style = list(color = "#000000", fontSize = "12px", fontFamily = "Montserrat")
#   #   )
#   # )
#   # 
#   # 
#   # ###################################
#   # 
#   # # ---- Choose dataset by level ----
#   # if (input$level_choice == "Continent") {
#   #   
#   #   data <- gadm_df_aqli_country_test() %>%
#   #     select("country","continent", all_of(starts_with("pm"))) %>%
#   #     setNames(c("country", "continent", as.character(1998:2024))) %>%
#   #     pivot_longer(!c(country, continent), names_to = "year", values_to = "value")
#   #   
#   # } else if (input$level_choice == "Country") {
#   #   
#   #   data <- gadm_df_aqli_state() %>%
#   #     select("name_1","continent","country", all_of(starts_with("pm"))) %>%
#   #     setNames(c("name_1", "continent","country", as.character(1998:2024))) %>%
#   #     pivot_longer(!c(continent,country,name_1), names_to = "year", values_to = "value")
#   #   
#   # } else if (input$level_choice == "State") {
#   #   
#   #   data <- gadm_df_aqli_district() %>%
#   #     select("name_2","continent","country","name_1", all_of(starts_with("pm"))) %>%
#   #     setNames(c("name_2","continent","country","name_1", as.character(1998:2024))) %>%
#   #     pivot_longer(!c(name_2,continent,country,name_1), names_to = "year", values_to = "value")
#   #   
#   # } else if (input$level_choice == "District") {
#   #   
#   #   data <- gadm_df_aqli_district() %>%
#   #     select("name_2","continent","country","name_1", all_of(starts_with("pm"))) %>%
#   #     setNames(c("name_2","continent","country","name_1", as.character(1998:2024))) %>%
#   #     pivot_longer(!c(name_2,continent,country,name_1), names_to = "year", values_to = "value")
#   # }
#   # 
#   # # ---- Filter year ----
#   # data <- data %>% filter(year %in% input$year_aq)
#   # 
#   # # ---- Define grouping variable ----
#   # x_var <- switch( input$level_choice,
#   #                 "Continent" = "continent",
#   #                 "Country"   = "country",
#   #                 "State"     = "name_1",
#   #                 "District"  = "name_2")
#   # 
#   # # ---- Classify countries above/below WHO ----
#   # data <- data %>%
#   #   mutate(
#   #     below_who = ifelse(value <= 5, 1, 0),
#   #     above_who = ifelse(value > 5, 1, 0)
#   #   )
#   # 
#   # df <- data %>%
#   #   group_by(!!sym(x_var)) %>%
#   #   summarise(
#   #     below_who = sum(below_who, na.rm = TRUE),
#   #     above_who = sum(above_who, na.rm = TRUE),
#   #     .groups = "drop"
#   #   )
#   # 
#   # print("Nothing to be get printing")
#   # print(df)
#   #   
#   # # ---- Highchart ----
#   # highchart() %>%
#   #   hc_chart(type = "bar") %>%
#   #   hc_title(
#   #     text = paste("Above and Below WHO PM2.5 Guideline -", input$year_aq),
#   #     style = list(fontFamily = "Montserrat")
#   #   ) %>%
#   #   hc_subtitle(text = paste("by", input$level_choice),
#   #               style = list(fontFamily = "Montserrat")) %>%
#   #   hc_xAxis(categories = df[[x_var]], title = list(text = input$level_choice)) %>%
#   #   hc_yAxis(min = 0, title = list(text = "Number of Units"),
#   #            stackLabels = list(enabled = TRUE,
#   #                               style = list(textOutline = "none", color = "#000000",
#   #                                            fontFamily = "Montserrat", fontWeight = "bold"))) %>%
#   #   hc_plotOptions(series = list(
#   #     stacking = "normal",
#   #     dataLabels = list(enabled = TRUE,
#   #                       style = list(textOutline = "none", color = "#FFFFFF",
#   #                                    fontSize = "11px", fontFamily = "Montserrat"))
#   #   )) %>%
#   #   hc_add_series(name = "Above WHO Guideline", data = df$above_who) %>%
#   #   hc_add_series(name = "Below WHO Guideline", data = df$below_who) %>%
#   #   hc_add_theme(hc_theme_aqli)
#   # 
#   # 
#   
# })


