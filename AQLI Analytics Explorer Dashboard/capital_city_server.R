# ============================================================
# Author:        Purushottam Gupta
# Organization:  Air Quality Life Index (AQLI)
#                University of Chicago
# Email:         guptap@uchicago.edu
#
# Description:
# Shiny reactive filtering system with cascading filters
# (Continent → Region → Country → District)
# ============================================================

# ---------------------------------------------------------
# Reactive values to store selections for population analysis
# (Continent → Region → Country → Capital/District)
# ---------------------------------------------------------

rv_continent_pop  <- reactiveValues()   # selected continent(s)
rv_aqregion_pop   <- reactiveValues()   # selected region(s)
rv_country_pop    <- reactiveValues()   # selected country(s)
# rv_state_pop    <- reactiveValues()   # state level not used currently
rv_district_pop   <- reactiveValues()   # selected capital city IDs


# ---------------------------------------------------------
# Initialize reactive values from UI inputs
# isolate() prevents reactive triggering during initialization
# ---------------------------------------------------------

isolate({
  
  rv_continent_pop$continent_pop <- input$continent_pop
  rv_aqregion_pop$region_pop     <- input$region_pop
  rv_country_pop$country_pop     <- input$country_pop
  # rv_state_pop$state_pop       <- input$state_pop
  rv_district_pop$district_pop   <- input$district_pop
  
  # rv_year_bs$year <- input$year
})


# ---------------------------------------------------------
# Update region choices when continent selection changes
# ---------------------------------------------------------

observeEvent(rv_continent_pop$continent_pop, {
  
  updatePickerInput(
    session,
    "region_pop",
    "Select Region(s):",
    
    choices = sort(unique(
      country_capital[continent %in% rv_continent_pop$continent_pop, region]
    )),
    
    selected = sort(unique(
      country_capital[continent %in% rv_continent_pop$continent_pop, region]
    ))
  )
})


# ---------------------------------------------------------
# Update country choices when region selection changes
# ---------------------------------------------------------

observeEvent(rv_aqregion_pop$region_pop, {
  
  updatePickerInput(
    session,
    "country_pop",
    "Select Country(s):",
    
    choices = sort(unique(
      country_capital[region %in% rv_aqregion_pop$region_pop, country]
    )),
    
    selected = sort(unique(
      country_capital[region %in% rv_aqregion_pop$region_pop, country]
    ))
  )
})


# ---------------------------------------------------------
# Update capital city choices when country selection changes
# ---------------------------------------------------------

observeEvent(rv_country_pop$country_pop, {  
  
  updatePickerInput(
    session,
    "district_pop",
    "Select Capital(s):",
    
    choices = sort(unique(
      country_capital[country %in% rv_country_pop$country_pop, capital_city_id]
    )),
    
    # Default selected capitals (South Asia major capitals)
    selected  = c(
      "NCT of Delhi(151)",
      "Dhaka(105)",
      "Islamabad(182)",
      "Colombo(201)",
      "Kathmandu(172)"
    )
  )
})


# ---------------------------------------------------------
# Observer to update reactive values only when
# dropdown menus are not actively open
# This prevents unwanted UI resets during selection
# ---------------------------------------------------------

observe({
  
  if(!isTRUE(input$continent_pop_open) &
     !isTRUE(input$region_pop_open) &
     !isTRUE(input$country_pop_open) &
     !isTRUE(input$district_pop_open))
  {
    
    rv_continent_pop$continent_pop <- input$continent_pop
    rv_aqregion_pop$region_pop     <- input$region_pop
    rv_country_pop$country_pop     <- input$country_pop
    rv_district_pop$district_pop   <- input$district_pop
    
  }
})


gadm_df_aqli_capital <- reactive({
  
  country_capital[
    (continent   %in%  rv_continent_pop$continent_pop ) &
      (region    %in%  rv_aqregion_pop$region_pop) &
      (country   %in%  rv_country_pop$country_pop ) &
      (capital_city_id  %in%  input$district_pop ) &
      (population >= input$pop_range_capital[1] ) &
      (population <= input$pop_range_capital[2] )
  ]
})


observe({
  
  print("gadm_df_aqli_capital")
#  print(gadm_df_aqli_capital())
  
})

output$capital_yr_wse_line_aqli <- renderHighchart({
  
  # Common setup
  colors <- c("#FF8C00", "#1F4E79", "#FFB84D", "#336699", "#FF9933") # dark orange & blue shades
  
  # Conditional setup
  if(input$aqli_comp_choice_cap == "pm_comp_25"){
    
    measure_prefix <- "pm"
    title_text <- "Country Capital- PM2.5 concentration"
    y_title <- "PM2.5 (µg/m³)"
    value_suffix <- " µg/m³"
    
    data <- gadm_df_aqli_capital() %>%
      select("Capital_city_region", "continent", "country", "region", all_of(starts_with(measure_prefix))) %>%
      setNames(c("Capital_city_region", "continent", "country", "region", as.character(1998:2024))) %>%
      pivot_longer(!c(Capital_city_region, continent, country, region), names_to = "year", values_to = "value")
    
  }
  else if(input$aqli_comp_choice_cap == "llpp_comp"){
    
    measure_prefix <- "llpp_who"
    title_text <- "Country Capital- Life Year Loss Per Person (WHO)"
    y_title <- "Life Years Lost"
    value_suffix <- " years"
    
    data <- gadm_df_aqli_capital() %>%
      select("Capital_city_region", "continent", "country", "region", all_of(starts_with(measure_prefix))) %>%
      setNames(c("Capital_city_region", "continent", "country", "region", as.character(1998:2024))) %>%
      pivot_longer(!c(Capital_city_region, continent, country, region), names_to = "year", values_to = "value")
    
  }
  else if(input$aqli_comp_choice_cap == "nat_llpp_comp"){
    
    measure_prefix <- "llpp_nat"
    title_text <- "Country Capital- Life Year Loss Per Person (Nat.)"
    y_title <- "Life Years Lost"
    value_suffix <- " years"
    
    data <- gadm_df_aqli_capital() %>%
      select("Capital_city_region", "continent", "country", "region", all_of(starts_with(measure_prefix))) %>%
      setNames(c("Capital_city_region", "continent", "country", "region", as.character(1998:2024))) %>%
      pivot_longer(!c(Capital_city_region, continent, country, region), names_to = "year", values_to = "value")
    
  }
  
  title_all <- paste0("<span style='color:maroon;'>", title_text, "</span>")
  
  # Years on X-axis
  years <- sort(unique(data$year))
  
  # Prepare dataset for trend chart
  trend_data <- data %>%
    arrange(Capital_city_region, year)
  
  # Unique categories (series)
  categories <- unique(trend_data[["Capital_city_region"]])
  
  # Highchart: Year-wise trend
  hc <- highchart() %>%
    hc_chart(type = "spline",
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
      title = list(text = y_title,
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
      filter(Capital_city_region == cat) %>%
      arrange(year)
    
    hc <- hc %>% hc_add_series(
      name = cat,
      data = df_cat$value,
      tooltip = list(valueSuffix = value_suffix),
      marker = list(enabled = TRUE)
    )
  }
  
  hc
  
})


output$data_above_and_below_who <- renderReactable({
  
  dt <- gadm_df_aqli_capital() 
  
  # 1. Select required columns
  base_select <- dt[, .SD, .SDcols = c("Capital_city_region","capital_city_id", "population", "continent",
                                                    "country",
                                                    grep("^pm", names(country_capital), value = TRUE))]
  
  # 2. Melt PM columns
  base_data <- melt(
    base_select,
    id.vars = c("Capital_city_region","capital_city_id","country", "population", "continent"),
    measure.vars = patterns("^pm"),
    variable.name = "year_str",
    value.name = "PM2.5"
  )[
    , year := as.numeric(gsub("pm", "", year_str))
  ][
    , category := fifelse(PM2.5 > who_guideline, "Above WHO", "Below WHO")
  ][
    !is.na(category)
  ]
  
  # 3. Aggregate
  data <- base_data[
    , .(
      tot_pop   = sum(population, na.rm = TRUE),
      PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE)
      # PM2.5_a = PM2.5
      
    ),
    by = .(continent, country, Capital_city_region, capital_city_id, year, category)
  ][
    , population := sum(tot_pop), by = .(continent, country, Capital_city_region, capital_city_id, year)
  ][
    , pct := (tot_pop / population) * 100
  ]
  
  # 4. Convert to wide-like structure for WHO categories
  df_wide <- data[
    , .(
      popAboveWHO = floor(sum(fifelse(category == "Above WHO", population, NA_real_), na.rm = TRUE)),
      popBelowWHO = floor(sum(fifelse(category == "Below WHO", population, NA_real_), na.rm = TRUE)),
      pctAboveWHO = floor(sum(fifelse(category == "Above WHO", pct, NA_real_), na.rm = TRUE)),
      pctBelowWHO = floor(sum(fifelse(category == "Below WHO", pct, NA_real_), na.rm = TRUE))
    ),
    by = .(country, Capital_city_region, year, tot_pop, PM2.5_avg)
  ]
  
  
  
  # Precompute normalized bar widths for faster rendering
  # (Do this once before creating the reactable)
  max_pop_above <- max(df_wide$popAboveWHO, na.rm = TRUE)
  max_pop_below <- max(df_wide$popBelowWHO, na.rm = TRUE)
  max_pm25 <- max(df_wide$PM2.5_avg, na.rm = TRUE)
  
  df_wide$popAboveWHO_bar <- (df_wide$popAboveWHO / max_pop_above) * 100
  df_wide$popBelowWHO_bar <- (df_wide$popBelowWHO / max_pop_below) * 100
  df_wide$PM2.5_avg_bar <- (df_wide$PM2.5_avg / max_pm25) * 100
  
  # Define colors as hex strings (no external dependencies)
  col_orange <- "#FF8C00"
  col_blue   <- "#0072B2"
  col_pm25   <- "#808080"
  
  # Helper function to format population numbers with separators
  format_pop <- function(value) {
    if (is.na(value)) return("")
    format(as.numeric(value), big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  
  reactable(
    df_wide %>% select(-c("popAboveWHO_bar", "popBelowWHO_bar", "PM2.5_avg_bar")) %>% filter(year %in% input$years_pop),
    pagination = TRUE,
    striped = TRUE,
    highlight = TRUE,
    bordered = FALSE,
    defaultPageSize = 20,
    theme = reactablefmtr::nytimes(),
    
    columns = list(
      country = colDef(name = "Country"),
      Capital_city_region = colDef(name = "Capital"),
      year = colDef(name = "Year"),
      
      # Bar shown (Gray) - Custom fast cell renderer with superimposed text
      PM2.5_avg = colDef(
        name = "PM2.5",
        align = "left",
        html = TRUE,
        cell = function(value, index) {
          
          if (is.na(value) || value == 0) return(sprintf("%.1f", value))
          
          bar_width <- df_wide$PM2.5_avg_bar[[index]]
          formatted_value <- sprintf("%.1f", value)
          
          sprintf(
            '<div style="width:100%%; display:flex; flex-direction:column; align-items:flex-start;">
         <span style="font-size:11px; font-weight:bold; margin-bottom:2px;">%s</span>
         <div style="width:100%%; background-color:#e5e5e5; height:10px; border-radius:2px; position:relative;">
           <div style="background-color:%s; height:10px; width:%.1f%%; border-radius:2px;"></div>
         </div>
       </div>',
            formatted_value, col_pm25, bar_width
          )
        }
      ),
      
      # Bar shown (Orange) - Custom fast cell renderer (updated height)
      popAboveWHO = colDef(
        name = "Pop Above WHO",
        align = "left",
        html = TRUE,
        cell = function(value, index) {
          
          if (is.na(value) || value == 0) return(format_pop(value))
          
          bar_width <- df_wide$popAboveWHO_bar[[index]]
          formatted_value <- format_pop(value)
          
          sprintf(
            '<div style="width:100%%; display:flex; flex-direction:column; align-items:flex-start;">
         <span style="font-size:11px; font-weight:bold; margin-bottom:2px;">%s</span>
         <div style="width:100%%; background-color:#f2f2f2; height:10px; border-radius:2px; position:relative;">
           <div style="background-color:%s; height:10px; width:%.1f%%; border-radius:2px;"></div>
         </div>
       </div>',
            formatted_value, col_orange, bar_width
          )
        }
      ),
      
      # Bar shown (Blue) - Custom fast cell renderer (updated height)
      popBelowWHO = colDef(
        name = "Pop Below WHO",
        align = "left",
        html = TRUE,
        cell = function(value, index) {
          
          if (is.na(value) || value == 0) return(format_pop(value))
          
          bar_width <- df_wide$popBelowWHO_bar[[index]]
          formatted_value <- format_pop(value)
          
          sprintf(
            '<div style="width:100%%; display:flex; flex-direction:column; align-items:flex-start;">
         <span style="font-size:11px; font-weight:bold; margin-bottom:2px;">%s</span>
         <div style="width:100%%; background-color:#f2f2f2; height:10px; border-radius:2px; position:relative;">
           <div style="background-color:%s; height:10px; width:%.1f%%; border-radius:2px;"></div>
         </div>
       </div>',
            formatted_value, col_blue, bar_width
          )
        }
      ),
      
      # No bar here
      pctAboveWHO = colDef(
        name = "% Above WHO",
        format = colFormat(suffix = "%", digits = 1)
      ),
      
      # No bar here
      pctBelowWHO = colDef(
        name = "% Below WHO",
        format = colFormat(suffix = "%", digits = 1)
      )
    )
  )
  
  
})




output$data_above_and_below_nat <- renderReactable({
  
  dt <- gadm_df_aqli_capital() 
  
  # 1. Select required columns
  base_select <- dt[, .SD, .SDcols = c("Capital_city_region","capital_city_id", "population", "continent",
                                       "country", "natstandard",
                                       grep("^pm", names(country_capital), value = TRUE))]
  
  # 2. Melt PM columns
  base_data <- melt(
    base_select,
    id.vars = c("Capital_city_region","capital_city_id","country", "population", "continent", "natstandard"),
    measure.vars = patterns("^pm"),
    variable.name = "year_str",
    value.name = "PM2.5"
  )[
    , year := as.numeric(gsub("pm", "", year_str))
  ][
    , category := fifelse(PM2.5 > natstandard, "Above Nat.", "Below Nat.")
  ][
    !is.na(category)
  ]
  
  # 3. Aggregate
  data <- base_data[
    , .(
      tot_pop   = sum(population, na.rm = TRUE),
      PM2.5_avg = weighted.mean(PM2.5, population, na.rm = TRUE)
      # PM2.5_a = PM2.5
      
    ),
    by = .(continent, country, Capital_city_region, capital_city_id, year, category)
  ][
    , population := sum(tot_pop), by = .(continent, country, Capital_city_region, capital_city_id, year)
  ][
    , pct := (tot_pop / population) * 100
  ]
  
  # 4. Convert to wide-like structure for Nat. categories
  df_wide <- data[
    , .(
      popAboveNat = floor(sum(fifelse(category == "Above Nat.", population, NA_real_), na.rm = TRUE)),
      popBelowNat = floor(sum(fifelse(category == "Below Nat.", population, NA_real_), na.rm = TRUE)),
      pctAboveNat = floor(sum(fifelse(category == "Above Nat.", pct, NA_real_), na.rm = TRUE)),
      pctBelowNat = floor(sum(fifelse(category == "Below Nat.", pct, NA_real_), na.rm = TRUE))
    ),
    by = .(country, Capital_city_region, year, tot_pop, PM2.5_avg)
  ]
  
  
  
  # Precompute normalized bar widths for faster rendering
  # (Do this once before creating the reactable)
  max_pop_above <- max(df_wide$popAboveNat, na.rm = TRUE)
  max_pop_below <- max(df_wide$popBelowNat, na.rm = TRUE)
  max_pm25 <- max(df_wide$PM2.5_avg, na.rm = TRUE)
  
  df_wide$popAboveNat_bar <- (df_wide$popAboveNat / max_pop_above) * 100
  df_wide$popBelowNat_bar <- (df_wide$popBelowNat / max_pop_below) * 100
  df_wide$PM2.5_avg_bar <- (df_wide$PM2.5_avg / max_pm25) * 100
  
  # Define colors as hex strings (no external dependencies)
  col_orange <- "#FF8C00"
  col_blue <- "#0072B2"
  col_pm25 <- "#808080"
  
  # Helper function to format population numbers with separators
  format_pop <- function(value) {
    if (is.na(value)) return("")
    format(as.numeric(value), big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  
  reactable(
    df_wide %>% select(-c("popAboveNat_bar", "popBelowNat_bar", "PM2.5_avg_bar")) %>% filter(year %in% input$years_pop),
    pagination = TRUE,
    striped = TRUE,
    highlight = TRUE,
    bordered = FALSE,
    defaultPageSize = 20,
    theme = reactablefmtr::nytimes(),
    
    columns = list(
      country = colDef(name = "Country"),
      Capital_city_region = colDef(name = "Capital"),
      year = colDef(name = "Year"),
      
      # Bar shown (Gray) - Custom fast cell renderer with superimposed text
      PM2.5_avg = colDef(
        name = "PM2.5",
        align = "left",
        html = TRUE,
        cell = function(value, index) {
          
          if (is.na(value) || value == 0) return(sprintf("%.1f", value))
          
          bar_width <- df_wide$PM2.5_avg_bar[[index]]
          formatted_value <- sprintf("%.1f", value)
          
          sprintf(
            '<div style="width:100%%; display:flex; flex-direction:column; align-items:flex-start;">
         <span style="font-size:11px; font-weight:bold; margin-bottom:2px;">%s</span>
         <div style="width:100%%; background-color:#e5e5e5; height:10px; border-radius:2px; position:relative;">
           <div style="background-color:%s; height:10px; width:%.1f%%; border-radius:2px;"></div>
         </div>
       </div>',
            formatted_value, col_pm25, bar_width
          )
        }
      ),
      
      # Bar shown (Orange) - Custom fast cell renderer (updated height)
      popAboveNat = colDef(
        name = "Pop Above Nat",
        align = "left",
        html = TRUE,
        cell = function(value, index) {
          
          if (is.na(value) || value == 0) return(format_pop(value))
          
          bar_width <- df_wide$popAboveNat_bar[[index]]
          formatted_value <- format_pop(value)
          
          sprintf(
            '<div style="width:100%%; display:flex; flex-direction:column; align-items:flex-start;">
         <span style="font-size:11px; font-weight:bold; margin-bottom:2px;">%s</span>
         <div style="width:100%%; background-color:#f2f2f2; height:10px; border-radius:2px; position:relative;">
           <div style="background-color:%s; height:10px; width:%.1f%%; border-radius:2px;"></div>
         </div>
       </div>',
            formatted_value, col_orange, bar_width
          )
        }
      ),
      
      # Bar shown (Blue) - Custom fast cell renderer (updated height)
      popBelowNat = colDef(
        name = "Pop Below Nat",
        align = "left",
        html = TRUE,
        cell = function(value, index) {
          
          if (is.na(value) || value == 0) return(format_pop(value))
          
          bar_width <- df_wide$popBelowNat_bar[[index]]
          formatted_value <- format_pop(value)
          
          sprintf(
            '<div style="width:100%%; display:flex; flex-direction:column; align-items:flex-start;">
         <span style="font-size:11px; font-weight:bold; margin-bottom:2px;">%s</span>
         <div style="width:100%%; background-color:#f2f2f2; height:10px; border-radius:2px; position:relative;">
           <div style="background-color:%s; height:10px; width:%.1f%%; border-radius:2px;"></div>
         </div>
       </div>',
            formatted_value, col_blue, bar_width
          )
        }
      ),
      
      # No bar here
      pctAboveNat = colDef(
        name = "% Above Nat",
        format = colFormat(suffix = "%", digits = 1)
      ),
      
      # No bar here
      pctBelowNat = colDef(
        name = "% Below Nat",
        format = colFormat(suffix = "%", digits = 1)
      )
    )
  )
  
  
}

  
  
  
  
)