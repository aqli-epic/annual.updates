
# rv_continent_pop  <- reactiveValues()
# rv_aqregion_pop   <- reactiveValues()
rv_country_pop    <- reactiveValues()
rv_state_pop      <- reactiveValues()
rv_district_pop   <- reactiveValues()


isolate({
  
  
  # rv_continent_pop$continent_pop <- input$continent_pop
  # rv_aqregion_pop$region_pop     <- input$region_pop
  rv_country_pop$country_pop     <- input$country_pop
  rv_state_pop$state_pop         <- input$state_pop
  rv_district_pop$district_pop   <- input$district_pop
  #  rv_year_bs$year           <- input$year
  
})


observeEvent( rv_country_pop$country_pop, {
  

  updatePickerInput(session,"state_pop", "Select State(s):",
                    
                    choices = sort(unique((select_filter[country %in% rv_country_pop$country_pop, name_1]))),
                    selected = sort(unique((select_filter[country %in% rv_country_pop$country_pop, name_1])))
  )
  
})


observeEvent( rv_state_pop$state_pop, {  
  
  updatePickerInput(session,"district_pop", "Select District(s):",
                    
                    choices = sort(unique((select_filter[name_1 %in% rv_state_pop$state_pop, name_2]))),
                    
                    selected  = sort(unique((select_filter[name_1 %in% rv_state_pop$state_pop, name_2])))
  )
  
  
})



observe({
  
  
  if(!isTRUE(input$country_pop_open) & !isTRUE(input$state_pop_open) & !isTRUE(input$district_pop_open) )
  {

    
    rv_country_pop$country_pop     <- input$country_pop
    rv_state_pop$state_pop         <- input$state_pop
    rv_district_pop$district_pop   <- input$district_pop
    
  }
  
})


# get_df <- reactive({
#   
#   # print("d")
#   
#   df_temp <- gadm2_aqli_2023
#   
#   return(df_temp %>% filter(
#     country %in%  rv_country_pop$country_pop ,
#     name_1 %in%   rv_state_pop$state_pop,
#     name_2 %in% rv_district_pop$district_pop
#   ))
#   
# })


