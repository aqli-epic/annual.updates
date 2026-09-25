ui <- fluidPage(
  
  useShinyjs(),
  theme = bs_theme(version = 5, base_font = font_google("Montserrat")),
  
  # Waiter loading screen
  use_waiter(),
  waiter_show_on_load(
    html = tagList(
      spin_1(),
      br(),
      h4("Air Quality Life Index Dashboard Loading ...",
         style = "color:#FFFFFF; font-family:Montserrat;")
    ),
    color = "maroon"
  ),
  #color = "#002D72"
  # CSS: Small Button Styling
  tags$head(tags$style(HTML("
    .btn-sm {
      padding: 3px 10px;
      font-size: 12px;
    }
  "))),
  
  # CSS: Responsive Two-Column Layout
  tags$style(HTML("
    .responsive-two-column {
      display: flex;
      flex-direction: row;
      gap: 20px;
    }
    .responsive-two-column > div {
      flex: 1;
    }
    @media (max-width: 768px) {
      .responsive-two-column {
        flex-direction: column;
      }
    }
  ")),
  
  # CSS: Navigation Menu (Desktop and Mobile)
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
      z-index: 10000;
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
      .desktop-menu { display: none !important; }
      .mobile-menu-icon { display: block; }
    }
    .desktop-menu a:hover,
    .mobile-menu a:hover {
      color: gold !important;
    }
  ")),
  
  # CSS: Dropdown and Checkbox Styling
  tags$style(HTML("
    #level_1_id + .dropdown-toggle,
    #level_2_id + .dropdown-toggle {
      background: orange !important;
      color: #fff !important;
      font-size: 14px !important;
      padding: 2px 4px !important;
      height: 28px !important;
    }
    #use_state, #use_district_id {
      width: 16px !important;
      height: 15px !important;
      transform: scale(0.8);
      margin-right: 4px;
    }
    #use_state + label,
    #use_district_id + label {
      font-size: 7px !important;
    }
  ")),
  
  # CSS: Modal Styling
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
      max-height: 60vh;
      overflow-y: auto;
      overflow-x: auto;
    }
    .modal-footer {
      background-color: #f9f9f9;
      padding: 10px 15px;
      border-top: 1px solid #ccc;
    }
    .close { font-size: 18px; }
  ")),
  tags$head(tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css")),
  
  # Header
  tags$div(
    style = "background-color: #800000; color: white; padding: 8px 20px; display: flex; justify-content: space-between; flex-wrap: wrap;",
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
    
    tags$div(tags$img(src = "https://res.cloudinary.com/diwsbenwr/image/upload/v1754500352/Screenshot_2025-08-06_at_10.37.47_PM_upj4ei.png", height = "60px")),
    
    tags$div(
      class = "desktop-menu",
      style = "display: flex; gap: 25px; font-size: 16px; font-weight: 500;",
      tags$a("Data Repository", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'index')", style = "color: grey; text-decoration: none;"),
      tags$a("Region Comparison", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'impacts')", style = "color: grey; text-decoration: none;"),
      
      tags$a("Country", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'about')", style = "color: grey; text-decoration: none;"),
      tags$a("Country Capital", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'popcal')", style = "color: grey; text-decoration: none;"),
      
      tags$a("GBD", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'facts')", style = "color: grey; text-decoration: none;"),
      tags$a("Factsheet", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'reports')", style = "color: grey; text-decoration: none;"),
      tags$a("About", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'news')", style = "color: grey; text-decoration: none;")
    ),
    
    tags$div(
      style = "display: flex; gap: 10px; align-items: center;",
      tags$img(src = "https://img.icons8.com/ios-filled/20/search--v1.png"),
      tags$img(
        class = "mobile-menu-icon",
        src = "https://img.icons8.com/ios-filled/20/menu.png",
        onclick = "var menu = document.getElementById('mobileTabs'); console.log('Toggling menu:', menu); menu.classList.toggle('show');"
      )
    ),
    
    tags$div(
      id = "mobileTabs",
      class = "mobile-menu",
      tags$a("Data Repository", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'index')"),
      tags$a("Region Comparison", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'impacts')"),
      
      tags$a("Country", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'about')"),
      tags$a("Country Capital", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'popcal')"),
      
      tags$a("GBD", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'facts')"),
      tags$a("Factsheet", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'reports')"),
      tags$a("About", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'news')")
    )
  ),
  
  ####### End
  # Main content
  tags$div(
    id = "main-content",
    style = "padding: 30px;",
    
    # INDEX SECTION with FILTERS
    hidden(tags$div(
      id = "index_section",
     # h2("The Index"),
      
      # Filters inside Index Section
      fluidRow(
        column(3,
               pickerInput("continent", 
                           choices = sort(unique(gadm2_aqli$continent)), multiple = TRUE,
                           options = list(
                             `actions-box` = TRUE,
                             size = 6,
                             title = "Select Continent",
                             `selected-text-format` = "count > 2",
                             `count-selected-text` = "{0} Continent Selected",
                             `none-selected-text` = "Continent Not Found",
                             `select-all-text` = "All",
                             `live-search`=TRUE,
                             liveSearchPlaceholder = "Continent"
                           ),
                           selected = c("Asia")
               )
        ),
        column(3,
        pickerInput("aqregion", 
                    choices = "", multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      size = 6,
                      title = "Select Region",
                      `selected-text-format` = "count > 2",
                      `count-selected-text` = "{0} Region Selected",
                      `none-selected-text` = "Region Not Found",
                      `select-all-text` = "All",
                      `live-search`=TRUE,
                      liveSearchPlaceholder = "Region"
                    ),
                    selected = ""
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
                             `none-selected-text` = "Country Not Found",
                             `select-all-text` = "All",
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
                             `none-selected-text` = "State Not Found",
                             `select-all-text` = "All",
                             `live-search`=TRUE,
                             liveSearchPlaceholder = "State"
                           ),
                           selected = ""
               )
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
        #                      `none-selected-text` = "District Not Found",
        #                      `select-all-text` = "All",
        #                      liveSearch = TRUE,
        #                      liveSearchPlaceholder = "District"
        #                    ),
        #                    selected = ""
        #        )
        # ),
        fluidRow(
          column(3,
                 pickerInput("year",
                             choices = "", multiple = TRUE,
                             options = list(
                               `actions-box` = TRUE,
                               size = 6,
                               title = "Select Year",
                               `selected-text-format` = "count > 2",
                               `count-selected-text` = "{0} Year Selected",
                               `none-selected-text` = "Year Not Found",
                               `select-all-text` = "All",
                               `live-search`=TRUE,
                               liveSearchPlaceholder = "Year"
                             ),
                             selected = ""
                 )
          ),
          
          column(3,
                 actionButton(
                   "apply_filters",
                   label = tags$span(icon("filter"), "Apply Filters"),
                   class = "btn btn-primary btn-block apply-btn"
                 )
          )
        ),
        
    
     br(),
     
     fluidRow(
       column(
         6,
         radioButtons(
           inputId = "gadm_choice",
           label = NULL,
           choices = setNames(
             c("gadm0_choice", "gadm1_choice", "gadm2_choice"),
             c("GADM Level 0", "GADM Level 1", "GADM Level 2")
           ),
           selected = "gadm0_choice",
           inline = TRUE
         )
       )
     ),     
      
      fluidRow(
        
        column(width = 12, offset = 0,
               
               reactableOutput("gadm_table", height = "auto", width = "auto")
               
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
     

    )),
    
    # Other hidden sections
    (tags$div(id = "about_section", 
                    fluidRow(
                      
                      column(3,
                             pickerInput(
                               inputId = "country_gis",
                               choices = sort(unique(gadm2_aqli$country)),
                               multiple = FALSE,
                               label = "Select Country:",
                               options = list(
                                 `actions-box` = TRUE,
                                 size = 6,
                                 title = "Select Country",
                                 `selected-text-format` = "count > 0",
                                 `count-selected-text` = "{0} Country Selected",
                                 `none-selected-text` = "Country Not Found",
                                 `select-all-text` = "All",
                                 `live-search` = TRUE,
                                 liveSearchPlaceholder = "Country"
                               ),
                               selected = "Nigeria"  # This sets country as default
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
                                           `none-selected-text` = "Year Not Found",
                                           `select-all-text` = "All",
                                           `live-search`=TRUE,
                                           liveSearchPlaceholder = "Year"
                                         ),
                                         selected = max(unique(gadm2_long$year))
                             )
                      ),
                      
                      column(
                        6,
                        radioGroupButtons(
                          inputId = "switch_btn",
                          label = "Select Map View:",
                          choices = setNames(
                            c("pm25", "llpp"),
                            c(HTML("PM<sub>2.5</sub> Concentration"), "Potential Gain in Life Expectancy")
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
                               fluidRow(
                                column(2,
                                       checkboxInput("use_state", "State", value = FALSE)
                                  # checkboxInput("use_district", "District(s)", value = FALSE)
                                  ),
                                 
                                column(2,
                                     #  checkboxInput("use_state", "State(s)", value = FALSE),
                                       checkboxInput("use_district_id", "District", value = FALSE)
                                ),
                                   column(4,
                                          conditionalPanel(
                                            condition ="input.use_state == true",
                                   pickerInput(
                                     inputId = "level_1_id",
                                     choices = "",
                                     multiple = FALSE,
                                   #  label = "Select State:",
                                     options = list(
                                       `actions-box` = TRUE,
                                       size = 6,
                                       title = "Select State(s):",
                                       `selected-text-format` = "count > 0",
                                       `count-selected-text` = "{0} State Selected",
                                       `none-selected-text` = "State Not Found",
                                       `select-all-text` = "All",
                                       `live-search` = TRUE,
                                       liveSearchPlaceholder = "State"
                                     ),
                                     selected = ""  # This sets country as default
                                   )
                                   )),

                                   
                                   column(4,
                                          conditionalPanel(
                                            condition = "input.use_district_id == true",
                                          pickerInput(
                                            inputId = "level_2_id",
                                            choices = "",
                                            multiple = FALSE,
                                            #label = "Select District(s):",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 6,
                                             # title = "Select Districts",
                                              `selected-text-format` = "count > 0",
                                              `count-selected-text` = "{0} Districts Selected",
                                              `none-selected-text` = "Districts Not Found",
                                              `select-all-text` = "All",
                                              `live-search` = TRUE,
                                              liveSearchPlaceholder = "Districts"
                                            ),
                                            selected = ""  # This sets country as default
                                          )
                                   ))),
                               
                               highchartOutput("line_pm_llppwho", height = "440px")
                             )
                      ),
                      column(
                        6,
                        wellPanel(
                          
                          checkboxInput("switch_gis", "Show Map", value = FALSE),
                          
                          conditionalPanel(
                            condition = "input.switch_gis == true",
                            leafletOutput("country_wise_pm_llp", height = "440px")
                          )

                        )
                      )
                   ),
                    br(),
                    br(),
              fluidRow(
                column(
                  6,
                  radioButtons(
                    inputId = "global_level_choice1",
                    label = NULL,
                    choices = setNames(
                      c("state_choice1", "district_choice1" ),
                      c("State", "District")
                    ),
                    selected = "state_choice1",
                    inline = TRUE
                  ),
                )
              ),
              

                    fluidRow(

                      column(6,
                             wellPanel(
                               tabsetPanel(
                                 type = "tabs",  # or "pills"
                                 
                                 tabPanel("PM2.5 Concentration (μg/m³)",
                                          highchartOutput("top_10_polluted_populated")
                                 ),
                                 tabPanel("WHO (PGLE)",
                                          highchartOutput("top_10_polluted_populated_who")
                                 ),
                                 tabPanel("National (PGLE)",
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
                               highchartOutput("top_10_populated", height = "440px")
                             )
                      )
                      
                
                    )
                    
           
           
           
           )),
    
    hidden(tags$div(id = "popcal_section", 
        
                    fluidRow(

                      column(3,
                             pickerInput("continent_pop", 
                                         choices = unique(country_capital$continent), multiple = TRUE,
                                         options = list(
                                           `actions-box` = TRUE,
                                           size = 6,
                                           title = "Select Continent",
                                           `selected-text-format` = "count > 2",
                                           `count-selected-text` = "{0} Continent Selected",
                                           `none-selected-text` = "Continent Not Found",
                                           `select-all-text` = "All",
                                           `live-search`=TRUE,
                                           liveSearchPlaceholder = "Continent"
                                         ),
                                         selected = unique(country_capital$continent)
                             )
                      ),
                      column(3,
                             pickerInput("region_pop", 
                                         choices = "", multiple = TRUE,
                                         options = list(
                                           `actions-box` = TRUE,
                                           size = 6,
                                           title = "Select Region",
                                           `selected-text-format` = "count > 2",
                                           `count-selected-text` = "{0} Region Selected",
                                           `none-selected-text` = "Region Not Found",
                                           `select-all-text` = "All",
                                           `live-search`=TRUE,
                                           liveSearchPlaceholder = "Region"
                                         ),
                                         selected = ""
                             )
                      ),
                      column(3,
                             pickerInput("country_pop", 
                                         choices = "", multiple = TRUE,
                                         options = list(
                                           `actions-box` = TRUE,
                                           size = 6,
                                           title = "Select Country",
                                           `selected-text-format` = "count > 2",
                                           `count-selected-text` = "{0} Country Selected",
                                           `none-selected-text` = "Country Not Found",
                                           `select-all-text` = "All",
                                           `live-search`=TRUE,
                                           liveSearchPlaceholder = "Country"
                                         ),
                                         selected = ""
                             )
                      ),
                      column(3,
                             pickerInput("district_pop", 
                                         choices = "", multiple = TRUE,
                                         options = list(
                                           `actions-box` = TRUE,
                                           size = 6,
                                           title = "Select Capital",
                                           `selected-text-format` = "count > 2",
                                           `count-selected-text` = "{0} Capital Selected",
                                           `none-selected-text` = "Capital Not Found",
                                           `select-all-text` = "All",
                                           `live-search`=TRUE,
                                           liveSearchPlaceholder = "Capital"
                                         ),
                                         selected = "",
                             )
                      )),
                      
                      fluidRow(
                      column(
                        9,
                        sliderInput(
                          inputId = "pop_range_capital",
                          label = "Population range:",
                          min = 0,
                          max = max(country_capital$population, na.rm = T),
                          value = c(0, max(country_capital$population, na.rm = T)),
                          step = 10000
                        )
                      ),
                      column(3, offset = 0,
                             pickerInput("years_pop",
                                         choices = sort(unique(gadm0_long$year)), 
                                         multiple = TRUE,
                                         options = list(
                                           `actions-box` = TRUE,
                                           size = 6,
                                           `style` = "btn-primary",
                                           title = "Select Year",
                                           `selected-text-format` = "count > 2",
                                           `count-selected-text` = "{0} Year Selected",
                                           `none-selected-text` = "Year Not Found",
                                           `select-all-text` = "All",
                                           `live-search`=TRUE,
                                           liveSearchPlaceholder = "Year"
                                         ),
                                         selected = c("2024"),
                                         
                             )
                      ),
                      
                      
                      
                      
                    ),
                    
                    fluidRow(
                      
                      column(6,
                             wellPanel(
                               
                               column(12,
                                      # checkboxInput("use_bar_col", "Line Chart", value = FALSE),
                                      #checkboxInput("use_district", "District(s)", value = FALSE)
                                      prettyRadioButtons(
                                        inputId = "aqli_comp_choice_cap",
                                        label = NULL,
                                        choices = setNames(
                                          c("pm_comp_25", "llpp_comp", "nat_llpp_comp"),
                                          c(HTML("PM2.5 Concentration"), 
                                            "PGLE (WHO)", 
                                            "PGLE (Nat)")
                                        ),
                                        selected = "pm_comp_25",  # first option selected by default
                                        inline = TRUE,             # display horizontally
                                        status = "primary",        # color style
                                        shape = "round",           # radio button shape
                                        animation = "smooth",      # optional animation
                                        bigger = TRUE              # larger buttons
                                      )                             
                                      
                               ),
                               
                       
                                          highchartOutput("capital_yr_wse_line_aqli",height  = "460px")
                                 )

                             ),
                      
                      column(6,
                             wellPanel(
                               tabsetPanel(
                                 type = "tabs",  # or "pills"
                                 
                                 tabPanel("DataTable(Nat.)",
                                          reactableOutput("data_above_and_below_nat")
                                 ),
                                 tabPanel("DataTable(WHO)",
                                          reactableOutput("data_above_and_below_who")
                                 ),
                                 # tabPanel("National (PGLE)",
                                 #          highchartOutput("top_10_polluted_populated_nat")
                                 )
                               )
                             )
                      )
                      
                             
                    
                      
              
                    # column(
                    #   3,
                    #   sliderInput(
                    #     inputId = "pop_range_testing",
                    #     label = "Population range (in millions):",
                    #     min = 0,
                    #     max = ceiling(max(gadm0_aqli_2024$population, na.rm = TRUE)),
                    #     value = c(1, ceiling(max(gadm0_aqli_2024$population, na.rm = TRUE)), 
                    #     step = 1
                    #   )
                    # )
                    # )
                    
                    
                    
                                
    )),
    
    hidden(tags$div(id = "facts_section", 
                   # style = "padding: 30px;",
                    
                    fluidRow(
                      
                      column(3,
                             pickerInput(
                               inputId = "country_gis_gbd",
                               choices = sort(unique(gbd_results_master_2025$country)),
                               multiple = FALSE,
                               label = "Select Country:",
                               options = list(
                                 `actions-box` = TRUE,
                                 size = 6,
                                 title = "Select Country",
                                 `selected-text-format` = "count > 0",
                                 `count-selected-text` = "{0} Country Selected",
                                 `none-selected-text` = "Country Not Found",
                                 `select-all-text` = "All",
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
         
                   
    hidden(tags$div(id = "impacts_section", 
                    
                    tags$head(
                      tags$style(HTML("
                            #level_choice .pretty.p-default {
                              margin-top: -5px;
                            }
                            #level_choice label.control-label {
                              display: inline-block;
                              margin-right: 10px;
                              vertical-align: middle;
                            }
                            #level_choice .shiny-options-group {
                              display: inline-block;
                              vertical-align: middle;
                            }
                          "))
                    ),
                    
                    fluidRow(
                      useShinyjs(),  # enable shinyjs
                      
                      prettyRadioButtons(
                        inputId = "level_choice",
                        label = "Select Level:",
                        choices = c("Continent", "AQLI Region", "Country", "State", "District"),
                        selected = "Continent",   # default
                        inline = TRUE
                      )
                    ),
                    
                    
                    fluidRow(
                      column(3,
                             pickerInput(
                               "continent_aq", 
                               choices = NULL, 
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE,
                                 size = 6,
                                 title = "Select Continent",
                                 `selected-text-format` = "count > 2",
                                 `count-selected-text` = "{0} Continent Selected",
                                 `none-selected-text` = "Continent Not Found",
                                 `select-all-text` = "All",
                                 `live-search` = TRUE,
                                 liveSearchPlaceholder = "Continent",
                                 maxOptions = 4        # <-- LIMIT selection to 4
                               ),
                               selected = NULL) # optional: preselect first 4
                             
                      ),
                      column(3,
                             pickerInput(
                               "region_aq", 
                               choices = NULL, 
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE,
                                 size = 6,
                                 title = "Select Region",
                                 `selected-text-format` = "count > 2",
                                 `count-selected-text` = "{0} Region Selected",
                                 `none-selected-text` = "Region Not Found",
                                 `select-all-text` = "All",
                                 `live-search` = TRUE,
                                 liveSearchPlaceholder = "Region",
                                 maxOptions = 4        # <-- LIMIT selection to 4
                               ),
                               selected = NULL) # optional: preselect first 4
                             
                      ),
                      
                      column(3,
                             pickerInput("country_aq", 
                                         choices = NULL, multiple = TRUE,
                                         options = list(
                                           `actions-box` = TRUE,
                                           size = 6,
                                           title = "Select Country",
                                           `selected-text-format` = "count > 2",
                                           `count-selected-text` = "{0} Country Selected",
                                           `none-selected-text` = "Country Not Found",
                                           `select-all-text` = "All",
                                           `live-search`=TRUE,
                                           liveSearchPlaceholder = "Country"
                                         )
                                        # selected = ""
                             )
                      ),
                      column(3,
                             pickerInput("state_aq", 
                                         choices = NULL, multiple = TRUE,
                                         options = list(
                                           `actions-box` = TRUE,
                                           size = 6,
                                           title = "Select State",
                                           `selected-text-format` = "count > 2",
                                           `count-selected-text` = "{0} State Selected",
                                           `none-selected-text` = "State Not Found",
                                           `select-all-text` = "All",
                                           `live-search`=TRUE,
                                           liveSearchPlaceholder = "State"
                                         ),
                                      #   selected = ""
                             )
                      )),
                      fluidRow(
                        column(3,
                               pickerInput("district_aq",
                                           choices = NULL, multiple = TRUE,
                                           options = list(
                                             `actions-box` = TRUE,
                                             size = 6,
                                             title = "Select District",
                                             `selected-text-format` = "count > 0",
                                             `count-selected-text` = "{0} District Selected",
                                             `none-selected-text` = "District Not Found",
                                             `select-all-text` = "All",
                                             `live-search`=TRUE,
                                             liveSearchPlaceholder = "District"
                                           ),
                                           # selected = ""
                               )
                        ),
                        
                        
                      column(3,
                             pickerInput("year_aq",
                                         choices = year_for_filter, multiple = TRUE,
                                         options = list(
                                           `actions-box` = TRUE,
                                           size = 6,
                                           title = "Select Year",
                                           `selected-text-format` = "count > 2",
                                           `count-selected-text` = "{0} Year Selected",
                                           `none-selected-text` = "Year Not Found",
                                           `select-all-text` = "All",
                                           `live-search`=TRUE,
                                           liveSearchPlaceholder = "Year"
                                         ),
                                         selected = max(year_for_filter, na.rm = T)
                             )
                      ),
                      # column(
                      #   3,
                      #   sliderInput(
                      #     inputId = "pop_range",
                      #     label = "Population range (in millions):",
                      #     min = 0,
                      #     max = ceiling(max(continent_weighted$tot_population, na.rm = TRUE)),
                      #     value = c(1, ceiling(max(continent_weighted$tot_population, na.rm = TRUE))),
                      #     step = 1000000
                      #   )
                      # )

                      column(
                        4,
                          sliderInput(
                            inputId = "pop_range",
                            label = "Population range:",
                            min = 0,
                            max = 100,
                            value = c(0, 100),
                            step = 1000000
                          )
                      )
                      
                      
                      # column(3,
                      # 
                      #        compact_value_box(
                      #          "Population Affected", "512M",
                      #          icon = tags$i(class="bi bi-people-fill")
                      #        )
                      #        
                      #                              )
                      
                      
                      ),
                    
                    
                    # fluidRow(
                    #   column(6,
                    # 
                    #          # radioGroupButtons(
                    #          #   inputId = "aqli_comp_choice",
                    #          #   label = NULL,
                    #          #   choices = setNames(
                    #          #     c("pm_comp_25", "llpp_comp", "nat_llpp_comp"),
                    #          #     c(HTML("PM<sub>2.5</sub> Concentration"), "Life Expectancy Loss (WHO)", "Life Expectancy Loss (Nat)")
                    #          #   ),
                    #          #   justified = T,
                    #          #   status = "default",
                    #          #   # checkIcon = list(
                    #          #   #   yes = icon("square-check"),
                    #          #   #   no = icon("square")
                    #          #   # ),
                    #          #   direction = "horizontal",
                    #          #   
                    #          # ),
                    #   )
                    #   # column(5,
                    #   #        compact_value_box(
                    #   #          "Population Affected", "512M",
                    #   #          icon = tags$i(class="bi bi-people-fill")
                    #   #        )
                    #   # )
                    #   ),                    
                    
                    # fluidRow(
                    #   column(4, card_value_box("PM2.5 Level", "78 µg/m³", icon = tags$i(class="bi bi-cloud-haze2-fill"), subtitle="Year 2024")),
                    #   column(4, card_value_box("Life Expectancy Loss", "6.2 years", icon = tags$i(class="bi bi-heart-pulse-fill"), subtitle="AQLI Estimate")),
                    #   column(4, card_value_box("Population Affected", "512 M", icon = tags$i(class="bi bi-people-fill"), subtitle="At-risk population"))
                    # ),
                    
                    
                    fluidRow(
                      
                      column(6,
                             wellPanel(
                               
                               column(12,
                                     # checkboxInput("use_bar_col", "Line Chart", value = FALSE),
                                      #checkboxInput("use_district", "District(s)", value = FALSE)
                                      prettyRadioButtons(
                                        inputId = "aqli_comp_choice",
                                        label = NULL,
                                        choices = setNames(
                                          c("pm_comp_25", "llpp_comp", "nat_llpp_comp"),
                                          c(HTML("PM2.5 Concentration"), 
                                            "PGLE (WHO)", 
                                            "PGLE (Nat)")
                                        ),
                                        selected = "pm_comp_25",  # first option selected by default
                                        inline = TRUE,             # display horizontally
                                        status = "primary",        # color style
                                        shape = "round",           # radio button shape
                                        animation = "smooth",      # optional animation
                                        bigger = TRUE              # larger buttons
                                      )                             
                                      
                               ),
                               
                               tabsetPanel(
                                 type = "tabs",  # or "pills"
                                 tabPanel("Geography Wise", 
                                          
                                          highchartOutput("bar_aqli_comp_pm",height  = "460px")
                                 ),
                                 tabPanel("Year Wise",
                                          highchartOutput("yr_wse_line_aqli",height  = "460px")
                                 ),
                                 # tabPanel("National (PGLE)",
                                 #          highchartOutput("top_10_polluted_populated_nat")
                                 # )
                               )
                                                            )
                             
                             ),
                      column(6,
                             wellPanel(
                               fluidRow(
                                 
                                 column(4, 
                                        checkboxInput("use_nnum_perc", "Population (%)", value = FALSE)
                                        )
                                 # column(4,
                                 #        pill_value_box("bi bi-cloud-haze2-fill", "78 µg/m³", "PM2.5 (2024)")
                                 #        
                                 #       
                                 # ),
                                 # column(4,
                                 #        pill_value_box("bi bi-cloud-haze2-fill", "78 µg/m³", "PM2.5 (2024)")
                                 #        
                                 #        
                                 # )
                                 
                                 
                               ),
                               tabsetPanel(
                                 type = "tabs",  # or "pills"
                                 tabPanel("PM2.5 Exposure (Nat)", 

                                          highchartOutput("pop_below_above_nat",height  = "460px")
                                 ),
                                 tabPanel("PM2.5 Exposure (WHO)",
                                          highchartOutput("pop_below_above_who",height  = "460px")
                                 ),
                                 # tabPanel("National (PGLE)",
                                 #          highchartOutput("top_10_polluted_populated_nat")
                                 # )
                               )
                             )
                      )
                      
                      
                      # column(6,
                      #        
                      #     wellPanel(
                      #        highchartOutput("bar_aqli_comp_llwp")
                      #     )
                      #        
                      # )
                      
                      
                    ),
                    
                    br(),
                    # fluidRow(
                    #   column(6,
                    #          wellPanel(
                    #            plotOutput("region_with_below_and_above",height  = "500px")
                    #          )
                    #          
                    #   )
                    #   
                    # 
                    # 
                    # )
                    
                      
                    
                  
                    
                    
                    )),
          


    hidden(tags$div(id = "reports_section", 
                    
                    tags$iframe(
                      src = "factsheet.html",
                      style = "width:100%; height:100vh; border:none;"
                    )
                    
                    # fluidRow(
                    #   column(width = 12, class = "mb-3",
                    #          fluidRow(
                    #            column(width = 12, class = "col-md-3 col-12 mb-2",
                    #                   pickerInput(
                    #                     inputId = "country_gis_fs",
                    #                     choices = sort(unique(gbd_results_master_2025$country)),
                    #                     multiple = FALSE,
                    #                     label = "Select Country:",
                    #                     options = list(
                    #                       `actions-box` = TRUE,
                    #                       size = 6,
                    #                       title = "Select Country",
                    #                       `selected-text-format` = "count > 0",
                    #                       `count-selected-text` = "{0} Country Selected",
                    #                       `none-selected-text` = "Country Not Found",
                    #                       `select-all-text` = "All",
                    #                       `live-search` = TRUE,
                    #                       liveSearchPlaceholder = "Country"
                    #                     ),
                    #                     selected = "India"
                    #                   )
                    #            ),
                    #            column(width = 12, class = "col-md-3 col-12 mb-2",
                    #                   radioGroupButtons(
                    #                     inputId = "switch_over_btn_fs",
                    #                     label = "Select View:",
                    #                     choices = setNames(
                    #                       c("tblfs", "nrmfs"),
                    #                       c(HTML("Table"), "Normal")
                    #                     ),
                    #                     selected = "tblfs",
                    #                     justified = TRUE,
                    #                     checkIcon = list(yes = icon("check"))
                    #                   )
                    #            )
                    #          )
                    #   )
                    # ),
                    
          #           fluidRow(
          #             column(12,
          #                    tags$head(tags$style(HTML("
          #   #summary_text {
          #     font-family: Arial, sans-serif;
          #     font-size: 16px;
          #     line-height: 1.5;
          #     color: #333;
          #   }
          #   #summary_text b {
          #     color: #2c3e50;
          #     font-weight: 700;
          #   }
          #   .section-header {
          #     font-size: 18px;
          #     margin-top: 10px;
          #     margin-bottom: 5px;
          #   }
          # "))),
          #                    htmlOutput("summary_text")
          #             )
          #           ),
                    
                    # fluidRow(
                    #   column(6,
                    #          wellPanel(
                    #          plotOutput("fs_fig1_plot", height = "500px"),
                    #          downloadButton("download_png", "Download PNG"),
                    #          downloadButton("download_svg", "Download SVG")
                    #   )
                    #   )
                    #   
                    #   
                    #   
                    # )
                    

              

                    
                    
                    
                    
                    
                    )),
    
    
    hidden(tags$div(id = "news_section",               
                   # includeHTML("www/about_aqli.html")
                   tags$iframe(
                     src = "about_aqli.html",
                     style = "width:100%; height:100vh; border:none;"
                   )
                   
))
    #print("ui done ---------------------------")
  )
  )
  
ui <- secure_app(ui,
                 fab_button( position =  "bottom-right", label = "Logout"),
                 tags_top =
                   tags$div(
                    # tags$h4("AQLI Dashboard", style = "align:center"),
                     tags$img(
                       src = "epic-aqli-logo.png", width = 400
                     )
                   ),
                 #background  = "linear-gradient( rgba(99, 203, 255,0), rgba(99, 203, 255,1)),url('https://i.ibb.co/CWcsc4m/brlps-wall.png');"
                 #background  = "linear-gradient( rgba(99, 203, 255,0), rgba(99, 203, 255,1)),url('https://s3.gifyu.com/images/brlps_tattva_new_gif.gif');"
                                             
                 # style = sprintf(backgroundImageCSS,  "https://images.plot.ly/language-icons/api-home/r-logo.png")
                 background = "
                            radial-gradient(circle at top left, #002D72, #F26B38);
                            background-size: cover;
                          ",
                                         
                 tags_bottom = tags$div(
                   tags$p(
                     "For any assistance, please  contact ",
                     tags$a(
                       href = "mailto:guptap@uchicago.edu?Subject=AQLI%20Dashboard",
                       target="_top", "administrator"
                     )
                   )
                 )
                 
                 
                 
)

