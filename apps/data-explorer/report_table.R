
  

  
  rv_continent_bs <- reactiveValues()
  rv_country_bs   <- reactiveValues()
  rv_state_bs     <- reactiveValues()
  #rv_district_bs  <- reactiveValues()
  rv_year_bs     <- reactiveValues()
  
  
  isolate({
    
    
    rv_continent_bs$continent <- input$continent
    rv_country_bs$country     <- input$country
    rv_state_bs$state         <- input$state
   # rv_district_bs$district   <- input$district
    rv_year_bs$year         <- input$year

    
    
    
  })
  

  observeEvent(input$apply_filters,{
  observeEvent(
 
    rv_continent_bs$continent,{
      
      
      #setkeyv(gadm2_aqli_2023, c("DISTRICT_NAME_ID"))
      
      updatePickerInput(session,"country", "Select Country(s):",
                        
                        choices = sort(unique(
                          gadm2_long[continent %in% rv_continent_bs$continent, (country)]
                          
                        )),
                        selected = sort(unique(
                          gadm2_long[continent %in% gadm2_long$continent, (country)]

                        ))
      )
    })
  ################################################################################################
  observeEvent(
    # input$continent
    rv_country_bs$country ,{
      
      print("111111111111111111")
      
     # setkeyv(bank_sakhi_df, c("DISTRICT_NAME_ID"))
      
      updatePickerInput(session,"state", "Select State(s):",
                        
                        choices = sort(unique(
                          gadm2_long[country %in% rv_country_bs$country, name_1]
                          
                        )),
                        selected = sort(unique(
                          
                          gadm2_long[country %in% rv_country_bs$country, name_1]
                          
                        ))
      )
      
      print("am here in obsevent dist bs DIST OBS EVENT")
      
      
    })
  
  
  # observeEvent(
  #   # input$continent
  #   rv_state_bs$state,{
  #     
  #     
  #     ######## PANCHAYAT PICKER STARTS
  #     
  #     #setkey(bank_sakhi_filter, DISTRICT_NAME_ID, BLOCK_NAME_ID)
  #     #sel.col <- c("continent_with_id", "country_with_id")
  #     
  #     print("222222222222222222222")
  #     
  #     updatePickerInput(session,"district", "Subnational Unit (s):",
  #                       
  #                       choices = sort(unique(
  #                         
  #                         gadm2_long[name_1 %in% rv_state_bs$state, name_2]
  #                         
  #                       )),
  #                       
  #                       selected = sort(unique(
  #                         
  #                         gadm2_long[name_1 %in% rv_state_bs$state, name_2]
  #                         
  #                       ))
  #     )
  #   })
  
  observeEvent(
    # input$continent
    rv_state_bs$state,{
      
      updatePickerInput(session,"year", "Year(s):",
                        
                        choices = sort(unique(
                          
                          gadm2_long[name_1 %in% rv_state_bs$state, year]
                          
                        )),
                        
                        selected = 2023
      )
    })
  
  

  

  })
  
  
  observe({
    
    print("im in observe3333333333333333")
    if(!isTRUE(input$continent_open) & !isTRUE(input$country_open) & !isTRUE(input$state_open)  & !isTRUE(input$year_open))
      #& !isTRUE(input$shelter_unit_id_bs_open) & !isTRUE(input$pg_id_bs_open)) #& !isTRUE(input$category__bsopen) & !isTRUE(input$vaccine_bs_open) & !isTRUE(input$model_name_bs_open) )
    {
      print("I AM OBSERVER bs HERE ***************************************++++++++++++++++++++++++")
      
      rv_continent_bs$continent <- input$continent
      rv_country_bs$country     <- input$country
      rv_state_bs$state         <- input$state
     # rv_district_bs$district   <- input$district
      rv_year_bs$year         <- input$year
      
  
      
    }
    
  })
  
  # 

  

  
  gadm2_df_filter <- reactive({


    get_data_df <- gadm2_long
    return(get_data_df[continent %in% rv_continent_bs$continent &
                         country %in% rv_country_bs$country &
                         name_1 %in% rv_state_bs$state &
                     #   name_2 %in%  rv_district_bs$district &
                         year %in% rv_year_bs$year




                       ,])

    print("im in reactive2")
  })

observe({
  print("===============================================")
  print(gadm2_df_filter())
  print(rv_continent_bs$continent)
  print("country======================================")
  print(rv_country_bs$country)
  print("state======================================")
  print(rv_state_bs$state)
  
  print("year======================================")
  print(rv_year_bs$year)

})

output$gadm2_table <- renderReactable({
  
  data <- as.data.table(gadm2_df_filter())
  
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
      Region = colDef(name = "Region"),
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
  
})


# CSV Download
output$download_csv <- downloadHandler(
  filename = function() {
    paste0("gadm2_data_", Sys.Date(), ".csv")
  },
  content = function(file) {
    data <- as.data.table(gadm2_df_filter())
    data.table::fwrite(data, file)
  }
)

# Excel Download
output$download_excel <- downloadHandler(
  filename = function() {
    paste0("gadm2_data_", Sys.Date(), ".xlsx")
  },
  content = function(file) {
    data <- as.data.table(gadm2_df_filter())
    writexl::write_xlsx(data, path = file)
  }
)


###################################################################### 



############################################# End 





    


