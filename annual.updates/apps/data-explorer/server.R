server <- function(input, output, session) {
  observeEvent(input$tab_selected, {
    hide("index_section")
    hide("about_section")
    hide("facts_section")
    hide("impacts_section")
    hide("reports_section")
    hide("news_section")
    show(paste0(input$tab_selected, "_section"))
  })
  
  
  
  source("./report_table.R", local = TRUE)$value
  source("./gis.R", local = TRUE)$value
  source("./gbd_server.R", local = TRUE)$value
  
  
  
  
  
  
  
  
  
  
  
  
}