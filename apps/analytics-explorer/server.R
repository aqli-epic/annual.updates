# ============================================================
# Author:        Purushottam Gupta
# Organization:  Air Quality Life Index (AQLI)
#                University of Chicago
# Email:         guptap@uchicago.edu
#
# Description:
# Main Shiny server logic for the AQLI dashboard.
# Handles authentication, UI navigation, module loading,
# and initialization of supporting server-side scripts.
# ============================================================


server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # Simulate loading time (e.g., large datasets, models, etc.)
  # This allows the preloader animation to appear while the
  # application initializes.
  # ----------------------------------------------------------
  Sys.sleep(3)
  
  
  # ----------------------------------------------------------
  # Hide the loading screen once initialization is complete
  # ----------------------------------------------------------
  waiter_hide()
  
  
  # ----------------------------------------------------------
  # Enable full stack traces for debugging
  # Useful during development to identify error sources
  # ----------------------------------------------------------
  options(shiny.fullstacktrace = TRUE)
  
  
  # ----------------------------------------------------------
  # Dynamic navigation system
  # Shows the selected tab section and hides others
  # ----------------------------------------------------------
  observeEvent(input$tab_selected, {
    
    hide("index_section")
    hide("about_section")
    hide("facts_section")
    hide("impacts_section")
    hide("reports_section")
    hide("news_section")
    hide("popcal_section")
    
    # Show the selected section dynamically
    show(paste0(input$tab_selected, "_section"))
    
  })
  
  
  ###############################################################################
  #####################  Load Server Modules ####################################
  ###############################################################################
  
  # ----------------------------------------------------------
  # Source server-side scripts for modular functionality
  # Each script contains server logic for a specific feature
  # ----------------------------------------------------------
  
  source("./report_table.R", local = TRUE)      # Report tables
  source("./gis.R", local = TRUE)               # GIS / map functionality
  source("./gbd_server.R", local = TRUE)       # Global Burden of Disease analysis
  source("./factsheet_server.R", local = TRUE) # Country factsheet generation
  source("./aqli_comperison.R", local = TRUE)  # AQLI comparison tools
  source("./capital_city_server.R", local = TRUE) # Capital city pollution analysis
  
  
  # ----------------------------------------------------------
  # User authentication system
  # Uses shinymanager secure_server for login validation
  # ----------------------------------------------------------
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials, passphrase = NULL)
  )
  
  
}