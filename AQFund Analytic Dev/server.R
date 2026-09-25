server <- function(input, output, session) {
  # No sodium → use check_credentials directly

  
  # Simulate loading (e.g., loading data, models, etc.)
  Sys.sleep(3)
  
  # Hide the preloader after everything is ready
  waiter_hide()
  
  
  options(shiny.fullstacktrace = TRUE)
  
  
  observeEvent(input$tab_selected, {
    hide("index_section")
    hide("about_section")
    # hide("facts_section")
    # hide("impacts_section")
    # hide("reports_section")
    hide("news_section")
    # hide("popcal_section")
    
    show(paste0(input$tab_selected, "_section"))
  })
  
###############################################################################
#####################  
  
 source("./report_table.R", local = TRUE)
 # source("./gis.R", local = TRUE)
 # source("./gbd_server.R", local = TRUE)
 # source("./factsheet_server.R", local = TRUE)
 # source("./aqli_comperison.R", local = TRUE)
 # source("./capital_city_server.R", local = TRUE)
  

  res_auth <- secure_server(
    check_credentials = check_credentials(credentials, passphrase = NULL)
  )
  
  
}