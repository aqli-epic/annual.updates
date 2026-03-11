# ============================================================
# Author:        Purushottam Gupta
# Organization:  Air Quality Life Index (AQLI)
#                University of Chicago
# Email:         guptap@uchicago.edu
#
# Description:
# Shiny reactive filtering system with cascading filters
# (Continent → Region → Country → State → Year) and an
# Apply button mechanism to control when filters are applied
# to the underlying datasets.
# ============================================================

# ------------------------------------------
# Store current filter selections in reactiveValues
# These act as temporary containers for filter inputs
# ------------------------------------------

rv_continent_bs   <- reactiveValues()
rv_aq_region_bs   <- reactiveValues()
rv_country_bs     <- reactiveValues()
rv_state_bs       <- reactiveValues()
#rv_district_bs   <- reactiveValues()   # district level filter (currently unused)
rv_year_bs        <- reactiveValues()


# ------------------------------------------
# Initialize reactiveValues with current UI inputs
# isolate() prevents triggering reactive dependencies
# ------------------------------------------
isolate({
  
  rv_continent_bs$continent <- input$continent
  rv_aq_region_bs$aqregion  <- input$aqregion
  rv_country_bs$country     <- input$country
  rv_state_bs$state         <- input$state
  #rv_district_bs$district  <- input$district
  rv_year_bs$year           <- input$year
  
})


# -------------------------------------------------
# 1. Cascade Filters (dynamic dropdown dependencies)
# Each filter updates the next level in the hierarchy
# -------------------------------------------------

# When continent changes → update AQ region choices
observeEvent(input$continent, {
  updatePickerInput(session, "aqregion",
                    choices  = sort(unique(gadm0_long[continent %in% input$continent, region])),
                    selected = sort(unique(gadm0_long[continent %in% input$continent, region]))
  )
})

# When AQ region changes → update country choices
observeEvent(input$aqregion, {
  updatePickerInput(session, "country", 
                    choices  = sort(unique(gadm0_long[region %in% input$aqregion, country])),
                    selected = sort(unique(gadm0_long[region %in% input$aqregion, country]))
  )
})

# When country changes → update state/province choices
observeEvent(input$country, {
  updatePickerInput(session, "state", 
                    choices  = sort(unique(gadm2_long[country %in% input$country, name1_id])),
                    selected = sort(unique(gadm2_long[country %in% input$country, name1_id]))
  )
})

# When state changes → update available years
observeEvent(input$state, {
  updatePickerInput(session, "year", 
                    choices  = sort(unique(gadm2_long[name1_id %in% input$state, year])),
                    selected = 2024
  )
})


# -------------------------------------------------
# 2. Observer to store filter values
# Updates stored filters when dropdowns are closed
# (prevents partial updates while user is selecting)
# -------------------------------------------------

observe({
  
  # Check that no picker dropdown is currently open
  if(!isTRUE(input$continent_open) &
     !isTRUE(input$aqregion_open)  &
     !isTRUE(input$country_open)   &
     !isTRUE(input$state_open)     &
     !isTRUE(input$year_open))
  {
    
    print("Filters stored after dropdown selection")
    
    rv_continent_bs$continent <- input$continent
    rv_aq_region_bs$aqregion  <- input$aqregion
    rv_country_bs$country     <- input$country
    rv_state_bs$state         <- input$state
    #rv_district_bs$district  <- input$district
    rv_year_bs$year           <- input$year
    
  }
  
})


# -------------------------------------------------
# 3. Apply button logic
# Filters are officially applied only when user clicks
# the 'Apply Filters' button
# -------------------------------------------------

filters_applied <- eventReactive(input$apply_filters, {
  list(
    continent = rv_continent_bs$continent,
    aqregion  = rv_aq_region_bs$aqregion,
    country   = rv_country_bs$country,
    state     = rv_state_bs$state,
    year      = rv_year_bs$year
  )
})


# -------------------------------------------------
# 4. Filtered datasets (reactive)
# These depend only on filters_applied(), ensuring
# data updates only after clicking Apply
# -------------------------------------------------

# GADM2 level data (district / county level)
gadm2_df_filter <- reactive({
  req(filters_applied())
  
  get_data_df <- gadm2_long
  
  get_data_df[
    continent %in% filters_applied()$continent &
      region    %in% filters_applied()$aqregion  &
      country   %in% filters_applied()$country   &
      name1_id  %in% filters_applied()$state     &
      year      %in% filters_applied()$year
  ]
})


# GADM1 level data (state / province level)
gadm1_df_filter <- reactive({
  req(filters_applied())
  
  get_data_df <- gadm1_long
  
  get_data_df[
    continent %in% filters_applied()$continent &
      region    %in% filters_applied()$aqregion  &
      country   %in% filters_applied()$country   &
      name1_id  %in% filters_applied()$state     &
      year      %in% filters_applied()$year
  ]
})


# GADM0 level data (country level)
gadm0_df_filter <- reactive({
  req(filters_applied())
  
  get_data_df <- gadm0_long
  
  get_data_df[
    continent %in% filters_applied()$continent &
      region    %in% filters_applied()$aqregion  &
      country   %in% filters_applied()$country   &
      year      %in% filters_applied()$year
  ]
})


output$gadm_table <- renderReactable({
  
  if(input$gadm_choice == "gadm2_choice"){
  
  data <- gadm2_df_filter()
  data <- data %>% select(-c("id", "name1_id","objectid_gadm2","iso_alpha3" ))
  data <- as.data.table(data)
  
  reactable(
    data,
    filterable = TRUE,
    #searchable = TRUE,
    pagination = TRUE,
    highlight = TRUE,
    defaultPageSize = 10,
    striped = TRUE,
    bordered = TRUE,
    theme = reactableTheme(
      color = "#333",
      backgroundColor = "#fafafa",
      borderColor = "#dfe2e5",
      stripedColor = "#f0f0f0",
      highlightColor = "#e3f2fd",
      cellPadding = "8px 12px",
      style = list(fontSize = "14px")
    ),
    columns = list(
      continent = colDef(name = "Continent"),
      region = colDef(name = "Region"),
      country = colDef(name = "Country"),
      name_1 = colDef(name = "State / Province"),
      name_2 = colDef(name = "Subnational Unit"),
      population = colDef(name = "Population"),
      
      year = colDef(name = "Year", align = "center"),
      whostandard = colDef(name = "WHO Std (µg/m³)", align = "center"),
      natstandard = colDef(name = "Nat. Std (µg/m³)", align = "center"),
      pm = colDef(
        name = "PM2.5 (µg/m³)",
        align = "right",
        format = colFormat(digits = 2)
        # Removed style/color scale
      ),
      llpp_who = colDef(
        name   = "Life Expectancy Loss (WHO)",
        align = "right",
        format = colFormat(digits = 2)
        # Removed style/color scale
      ),
      llpp_nat = colDef(
        name   = "Life Expectancy Loss (Nat)",
        align = "right",
        format = colFormat(digits = 2)
        # Removed style/color scale
      )
    )
  )
  } else if(input$gadm_choice == "gadm1_choice"){
  
    data <- gadm1_df_filter()
    data <- data %>% select(-c( "name1_id"))
    data <- as.data.table(data)
    
    # print("--------------------------------------------------")
    # print(data)
    reactable(
      data,
      filterable = TRUE,
      #searchable = TRUE,
      pagination = TRUE,
      highlight = TRUE,
      defaultPageSize = 10,
      striped = TRUE,
      bordered = TRUE,
      theme = reactableTheme(
        color = "#333",
        backgroundColor = "#fafafa",
        borderColor = "#dfe2e5",
        stripedColor = "#f0f0f0",
        highlightColor = "#e3f2fd",
        cellPadding = "8px 12px",
        style = list(fontSize = "14px")
      ),
      columns = list(
        continent = colDef(name = "Continent"),
        region = colDef(name = "Region"),
        country = colDef(name = "Country"),
        name_1 = colDef(name = "State / Province"),
        #name_2 = colDef(name = "Subnational Unit"),
        population = colDef(name = "Population"),
        
        year = colDef(name = "Year", align = "center"),
        whostandard = colDef(name = "WHO Std (µg/m³)", align = "center"),
        natstandard = colDef(name = "Nat. Std (µg/m³)", align = "center"),
        pm = colDef(
          name = "PM2.5 (µg/m³)",
          align = "right",
          format = colFormat(digits = 2)
          # Removed style/color scale
        ),
        llpp_who = colDef(
          name   = "Life Expectancy Loss (WHO)",
          align = "right",
          format = colFormat(digits = 2)
          # Removed style/color scale
        ),
        llpp_nat = colDef(
          name   = "Life Expectancy Loss (Nat)",
          align = "right",
          format = colFormat(digits = 2)
          # Removed style/color scale
        )
      )
    )
    
    
  } else{
  
    data <- gadm0_df_filter()
    # print("--------------------------------------------------")
    
    col_nms = c("continent",  "region", "country", "population", "whostandard", "natstandard",  "year", "pm",
      "llpp_who", "llpp_nat")
    
    data = data %>% select(col_nms)
    data <- as.data.table(data)
    
    data$country <- iconv(data$country, from = "latin1", to = "UTF-8")
    reactable(
      data,
      filterable = TRUE,
      #searchable = TRUE,
      pagination = TRUE,
      highlight = TRUE,
      defaultPageSize = 10,
      striped = TRUE,
      bordered = TRUE,
      theme = reactableTheme(
        color = "#333",
        backgroundColor = "#fafafa",
        borderColor = "#dfe2e5",
        stripedColor = "#f0f0f0",
        highlightColor = "#e3f2fd",
        cellPadding = "8px 12px",
        style = list(fontSize = "14px")
      ),
      columns = list(
        continent = colDef(name = "Continent"),
        region = colDef(name = "Region"),
        country = colDef(name = "Country"),
        population = colDef(name = "Population"),
        
        year = colDef(name = "Year", align = "center"),
        whostandard = colDef(name = "WHO Std (µg/m³)", align = "center"),
        natstandard = colDef(name = "Nat. Std (µg/m³)", align = "center"),
        pm = colDef(
          name = "PM2.5 (µg/m³)",
          align = "right",
          format = colFormat(digits = 2)
          # Removed style/color scale
        ),
        llpp_who = colDef(
          name   = "Life Expectancy Loss (WHO)",
          align = "right",
          format = colFormat(digits = 2)
          # Removed style/color scale
        ),
        llpp_nat = colDef(
          name   = "Life Expectancy Loss (Nat)",
          align = "right",
          format = colFormat(digits = 2)
          # Removed style/color scale
        )
      )
    )
    
    
    
    
}
  

})


# CSV Download
output$download_csv <- downloadHandler(
  
  filename = function() {
    if (input$gadm_choice == "gadm2_choice") {
      paste0("gadm2_data_", Sys.Date(), ".csv")
    } else if (input$gadm_choice == "gadm1_choice") {
      paste0("gadm1_data_", Sys.Date(), ".csv")
    } else {
      paste0("gadm0_data_", Sys.Date(), ".csv") # fallback
    }
  },
  
  content = function(file) {
    if (input$gadm_choice == "gadm2_choice") {
      data <- as.data.table(gadm2_df_filter())
    } else if (input$gadm_choice == "gadm1_choice") {
      data <- as.data.table(gadm1_df_filter())
    } else {
      data <- data.table(gadm0_df_filter())
    }
    
    data.table::fwrite(data, file)
  }
)

# Excel Download
output$download_excel <- downloadHandler(
  
  filename = function() {
    if (input$gadm_choice == "gadm2_choice") {
      paste0("gadm2_data_", Sys.Date(), ".xlsx")
    } else if (input$gadm_choice == "gadm1_choice") {
      paste0("gadm1_data_", Sys.Date(), ".xlsx")
    } else {
      paste0("gadm0_data_", Sys.Date(), ".xlsx")  # fallback
    }
  },
  
  content = function(file) {
    if (input$gadm_choice == "gadm2_choice") {
      data <- as.data.table(gadm2_df_filter())
    } else if (input$gadm_choice == "gadm1_choice") {
      data <- as.data.table(gadm1_df_filter())
    } else {
      data <- data.table(gadm0_df_filter())
    }
    
    writexl::write_xlsx(data, path = file)
  }
)


###################################################################### 
#



############################################# End 





    


