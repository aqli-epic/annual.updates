

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5, base_font = font_google("Montserrat")),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      .mobile-menu {
        display: flex;
        flex-direction: column;
        position: absolute;
        top: 100%;
        left: 0;
        width: 100%;
        background-color: #800000;
        text-align: center;
        padding: 0;
        z-index: 9999;
        transform: translateY(-100%);
        opacity: 0;
        transition: transform 0.4s ease, opacity 0.4s ease;
        pointer-events: none;
      }

      .mobile-menu.show {
        transform: translateY(0%);
        opacity: 1;
        pointer-events: auto;
      }

      .mobile-menu a {
        color: white;
        padding: 12px 0;
        display: block;
        font-weight: 500;
        font-size: 16px;
        text-decoration: none;
        border-bottom: 1px solid rgba(255, 255, 255, 0.1);
      }

      .mobile-menu-icon {
        display: none;
        cursor: pointer;
      }

      @media (max-width: 768px) {
        .desktop-menu {
          display: none !important;
        }
        .mobile-menu-icon {
          display: block;
        }
      }

      .desktop-menu a:hover,
      .mobile-menu a:hover {
        color: gold !important;
      }
    "))
  ),
  
  # Header
  tags$div(
    style = "background-color: #800000; color: white; padding: 8px 20px; display: flex; justify-content: space-between;",
    tags$div(
      style = "display: flex; align-items: center;",
      tags$img(src = "https://res.cloudinary.com/diwsbenwr/image/upload/v1750129379/uchicago_logo_cbiopy.png", height = "25px"),
      tags$span("THE UNIVERSITY OF CHICAGO", style = "margin-left: 10px; font-weight: bold;")
    ),
    tags$div("EPIC · UCHICAGO CLIMATE & GROWTH")
  ),
  
  # Navigation Bar
  tags$div(
    style = "background-color: white; padding: 10px 30px; display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid #ccc; position: relative;",
    
    tags$div(tags$img(src = "https://res.cloudinary.com/diwsbenwr/image/upload/v1750129467/aqli_logo_g2fu7o.png", height = "60px")),
    
    tags$div(
      class = "desktop-menu",
      style = "display: flex; gap: 25px; font-size: 16px; font-weight: 500;",
      tags$a("Reports", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'index')", style = "color: grey; text-decoration: none;"),
      tags$a("Tab1", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'about')", style = "color: grey; text-decoration: none;"),
      tags$a("Tab2", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'facts')", style = "color: grey; text-decoration: none;"),
      tags$a("Tab3", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'impacts')", style = "color: grey; text-decoration: none;"),
      tags$a("Tab4", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'reports')", style = "color: grey; text-decoration: none;"),
      tags$a("Tab5", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'news')", style = "color: grey; text-decoration: none;")
    ),
    
    tags$div(
      style = "display: flex; gap: 10px; align-items: center;",
      tags$img(src = "https://img.icons8.com/ios-filled/20/search--v1.png"),
     # tags$img(src = "https://img.icons8.com/ios-filled/20/twitter.png"),
      tags$img(
        class = "mobile-menu-icon",
        src = "https://img.icons8.com/ios-filled/20/menu.png",
        onclick = "var menu = document.getElementById('mobileTabs'); menu.classList.toggle('show');"
      )
    ),
    
      id = "mobileTabs",
    tags$div( 
      class = "mobile-menu",
      tags$a("The Index", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'index')"),
      tags$a("About", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'about')"),
      tags$a("Pollution Facts", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'facts')"),
      tags$a("Policy Impacts", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'impacts')"),
      tags$a("Reports", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'reports')"),
      tags$a("News", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'news')")
    )
  ),
  ############ Added for Modal Design #
  
  tags$head(
    tags$style(HTML("
    .modal-dialog.modal-balanced {
      max-width: 1000px;
      width: 90%;
      margin: 30px auto;
    }

    .modal-content {
      border-radius: 8px;
      box-shadow: 0 4px 30px rgba(0,0,0,0.2);
      overflow: hidden;
    }

    .modal-header {
      background-color: #8ed2ff;
      border-bottom: 1px solid #ccc;
      padding: 15px;
    }

    .modal-title {
      font-weight: bold;
      font-size: 22px;
      color: #2c3e50;
    }

    .modal-body {
      padding: 20px;
    }

    .modal-footer {
      background-color: #f9f9f9;
      padding: 10px 15px;
      border-top: 1px solid #ccc;
    }

    .close {
      font-size: 18px;
    }
    
    .modal-body {
  padding: 20px;
  max-height: 60vh; /* Limits the body height to 60% of viewport height */
  overflow-y: auto; /* Adds vertical scrollbar if content overflows */
  overflow-x: auto; /* Ensures horizontal scrollbar for wide tables */
}
  "))
  ),
  
  ####### End
  # Main content
  tags$div(
    id = "main-content",
    style = "padding: 30px;",
    
    # INDEX SECTION with FILTERS
    tags$div(
      id = "index_section",
     # h2("The Index"),
      
      # Filters inside Index Section
      fluidRow(
        column(3,
               pickerInput("continent", 
                           choices = sort(unique(gadm2_aqli_2023$continent)), multiple = TRUE,
                           options = list(
                             `actions-box` = TRUE,
                             size = 6,
                             title = "Select Continent",
                             `selected-text-format` = "count > 2",
                             `count-selected-text` = "{0} Continent Selected",
                             noneResultsText = "Continent Not Found",
                             selectAllText = "All",
                             `live-search`=TRUE,
                             liveSearchPlaceholder = "Continent"
                           ),
                           selected = sort(unique(gadm2_aqli_2023$continent))
               )
        ),
        column(3,
               pickerInput("country", 
                           choices = "", multiple = TRUE,
                           options = list(
                             `actions-box` = TRUE,
                             size = 6,
                             title = "Select Country",
                             `selected-text-format` = "count > 2",
                             `count-selected-text` = "{0} Country Selected",
                             noneResultsText = "Country Not Found",
                             selectAllText = "All",
                             `live-search`=TRUE,
                             liveSearchPlaceholder = "Country"
                           ),
                           selected = ""
               )
        ),
        column(3,
               pickerInput("state", 
                           choices = "", multiple = TRUE,
                           options = list(
                             `actions-box` = TRUE,
                             size = 6,
                             title = "Select State",
                             `selected-text-format` = "count > 2",
                             `count-selected-text` = "{0} State Selected",
                             noneResultsText = "State Not Found",
                             selectAllText = "All",
                             `live-search`=TRUE,
                             liveSearchPlaceholder = "State"
                           ),
                           selected = ""
               )
        ),
        column(3,
               pickerInput("year",
                           choices = "", multiple = TRUE,
                           options = list(
                             `actions-box` = TRUE,
                             size = 6,
                             title = "Select Year",
                             `selected-text-format` = "count > 2",
                             `count-selected-text` = "{0} Year Selected",
                             noneResultsText = "Year Not Found",
                             selectAllText = "All",
                             `live-search`=TRUE,
                             liveSearchPlaceholder = "Year"
                           ),
                           selected = ""
               )
        ),
        
        # column(3,
        #        pickerInput("district", 
        #                    choices = "", multiple = TRUE,
        #                    options = list(
        #                      `actions-box` = TRUE,
        #                      size = 6,
        #                      title = "Select District",
        #                      `selected-text-format` = "count > 0",
        #                      `count-selected-text` = "{0} District Selected",
        #                      noneResultsText = "District Not Found",
        #                      selectAllText = "All",
        #                      liveSearch = TRUE,
        #                      liveSearchPlaceholder = "District"
        #                    ),
        #                    selected = ""
        #        )
        # ),
        fluidRow(
          column(3,
                 actionButton(
                   "apply_filters",
                   label = tags$span(icon("filter"), "Apply Filters"),
                   class = "btn btn-primary btn-block apply-btn"
                 )
          )
        ),
        br(),
      ),
     br(),
      
      fluidRow(
        
        column(width = 12, offset = 0,
               
               reactableOutput("gadm2_table", height = "auto", width = "auto")
               
               )
        
        
        
      ),
     
     br(),
     
 
     
     
     fluidRow(
       column(
         width = 6,
         tags$div(
           style = "display: flex; gap: 10px;",
           downloadButton("download_csv", "Download CSV", class = "btn btn-primary btn-sm"),
           downloadButton("download_excel", "Download Excel", class = "btn btn-success btn-sm")
         )
       )
     ),
     
     
     
    # actionButton("Back", "Back", icon = icon("arrow-left"), class = "btn btn-secondary"),
     

    ),
    
    # Other hidden sections
    hidden(tags$div(id = "about_section", 
                    fluidRow(
                      
                      column(3,
                             pickerInput(
                               inputId = "country_gis",
                               choices = sort(unique(gadm2_aqli_2023$country)),
                               multiple = FALSE,
                               label = "Select Country:",
                               options = list(
                                 `actions-box` = TRUE,
                                 size = 6,
                                 title = "Select Country",
                                 `selected-text-format` = "count > 0",
                                 `count-selected-text` = "{0} Country Selected",
                                 noneResultsText = "Country Not Found",
                                 selectAllText = "All",
                                 `live-search` = TRUE,
                                 liveSearchPlaceholder = "Country"
                               ),
                               selected = "China"  # ✅ This sets China as default
                             )
                      ),
                      
    
           
                      column(3,
                             pickerInput("year_gis",
                                         label = "Select Year:",
                                         choices = sort(unique(gadm2_long$year)), multiple = FALSE,
                                         options = list(
                                           `actions-box` = TRUE,
                                           size = 6,
                                           title = "Select Year",
                                           `selected-text-format` = "count > 0",
                                           `count-selected-text` = "{0} Year Selected",
                                           noneResultsText = "Year Not Found",
                                           selectAllText = "All",
                                           `live-search`=TRUE,
                                           liveSearchPlaceholder = "Year"
                                         ),
                                         selected = sort(unique(gadm2_long$year))
                             )
                      ),
                      
                      column(
                        6,
                        radioGroupButtons(
                          inputId = "switch_btn",
                          label = "Select Map View:",
                          choices = setNames(
                            c("pm25", "llpp"),
                            c(HTML("PM<sub>2.5</sub> Concentration"), "Life Expectancy Loss")
                          ),
                          selected = "pm25",
                          # status = "default",
                          justified = TRUE,
                          checkIcon = list(yes = icon("check"))
                        )
                      )
                      
                      
                      
                      ),
                      


                      
                      # fluidRow(
                      # ),
                      
                      
                      
                      
                      
                      fluidRow(
                      
                      
                      column(6,
                             wellPanel(
                               # h4("Pollutants Status (Avg. Index)"),
                               highchartOutput("line_pm_llppwho")
                             )
                      ),
                      column(6,
                             wellPanel(
                               # tags$div(
                               #   tags$h4("Pollutant Exposure Status", class = "mb-2", style = "font-weight: 600; color: #333;"),
                               #   tags$p("Average annual PM₂.₅ concentration by selected regions", 
                               #          class = "text-muted", style = "margin-bottom: 15px; font-size: 14px;"),
                               leafletOutput("country_wise_pm_llp")
                               # )
                             )
                      )
                    ),
                    br(),
                    br(),
                    fluidRow(

                      column(6,
                             wellPanel(
                               tabsetPanel(
                                 type = "tabs",  # or "pills"

                                 tabPanel("PM2.5 Concentration (μg/m³)",
                                          highchartOutput("top_10_polluted_populated")
                                 ),
                                 tabPanel("WHO (LLPP)",
                                          highchartOutput("top_10_polluted_populated_who")
                                 ),
                                 tabPanel("National (LLPP)",
                                          highchartOutput("top_10_polluted_populated_nat")
                                 )
                               )
                             )
                      ),
                      
                      
                      # column(6,
                      #        wellPanel(
                      #          # h4("Pollutants Status (Avg. Index)"),
                      #          highchartOutput("top_10_polluted_populated")
                      #        )
                      # ),
                      column(6,
                             wellPanel(
                               # h4("Pollutants Status (Avg. Index)"),
                               highchartOutput("top_10_populated")
                             )
                      )
                      
                
                    )
                    
           
           
           
           )),
    hidden(tags$div(id = "facts_section", 
                   # style = "padding: 30px;",
                    
                    fluidRow(
                      
                      column(3,
                             pickerInput(
                               inputId = "country_gis_gbd",
                               choices = sort(unique(gbd_results_master_2023$country)),
                               multiple = FALSE,
                               label = "Select Country:",
                               options = list(
                                 `actions-box` = TRUE,
                                 size = 6,
                                 title = "Select Country",
                                 `selected-text-format` = "count > 0",
                                 `count-selected-text` = "{0} Country Selected",
                                 noneResultsText = "Country Not Found",
                                 selectAllText = "All",
                                 `live-search` = TRUE,
                                 liveSearchPlaceholder = "Country"
                               ),
                               selected = "China"  # ✅ This sets China as default
                             )
                      ),
                      chooseSliderSkin("Flat"),
                      
                  column(3,    
                      sliderInput(
                        inputId = "gbdslider", 
                        label = paste("Select Top Cause of Death:"),
                        min = 0, max = 25, value = 10
                      )
                  )
                      
                      
                      ),
                    
                    fluidRow(
                      
                      
                      # column(8,
                      #        wellPanel(
                      #          # h4("Pollutants Status (Avg. Index)"),
                      #          highchartOutput("gbd_top_10_risk")
                      #        )
                      # ),
                      
                      column(6,
                             wellPanel(
                               # h4("Pollutants Status (Avg. Index)"),
                               highchartOutput("top_10_gbd_cause")
                             )
                      )
                      
                      
                      )
                    
    )),
                    
                    
                   
                    
                    
                    
                   
    hidden(tags$div(id = "impacts_section", h2("Tab3"), plotlyOutput("impacts_plot"))),
    hidden(tags$div(id = "reports_section", h2("Tab4"), plotlyOutput("reports_plot"))),
    hidden(tags$div(id = "news_section", h2("Tab5"), plotlyOutput("news_plot"))),
    #print("ui done ---------------------------")
  )
  )
  

