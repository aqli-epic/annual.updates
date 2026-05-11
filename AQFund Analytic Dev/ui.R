ui <- fluidPage(
  
  useShinyjs(),
  theme = bs_theme(version = 5, base_font = font_google("Montserrat")),
  
  # Waiter loading screen
  use_waiter(),
  waiter_show_on_load(
    html = tagList(
      spin_1(),
      br(),
      h4("AQ Fund Dashboard Loading ...",
         style = "color:#FFFFFF; font-family:Montserrat;")
    ),
    color = "#39f"
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
  
  ######## Value Box
  # CSS: Enhanced Value Boxes
  tags$style(HTML("
    .value-box {
      border-radius: 12px;
      padding: 20px 22px;
      color: white;
      box-shadow: 0 6px 20px rgba(0, 0, 0, 0.18);
      height: 100%;
      position: relative;
      overflow: hidden;
      transition: all 0.3s ease;
    }
    .value-box:hover {
      transform: translateY(-5px);
      box-shadow: 0 10px 25px rgba(0, 0, 0, 0.25);
    }
    .value-box-title {
      font-size: 14.5px;
      font-weight: 500;
      margin-bottom: 6px;
      opacity: 0.95;
      letter-spacing: 0.3px;
    }
    .value-box-value {
      font-size: 32px;
      font-weight: 700;
      line-height: 1.05;
      margin: 0;
    }
    .value-box-icon {
      font-size: 48px;
      opacity: 0.18;
      position: absolute;
      top: 18px;
      right: 20px;
      transition: all 0.3s ease;
    }
    .value-box:hover .value-box-icon {
      opacity: 0.28;
      transform: scale(1.08);
    }
    .value-box small {
      font-size: 15px;
      font-weight: 500;
    }
  ")),
  ###################
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
    
    tags$div(tags$img(src = "https://res.cloudinary.com/diwsbenwr/image/upload/v1776844394/aqfundv23_eoieot.png", height = "50px")),
    
    tags$div(
      class = "desktop-menu",
      style = "display: flex; gap: 25px; font-size: 16px; font-weight: 500;",
      tags$a("Country Overview", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'index')", style = "color: grey; text-decoration: none;"),
      tags$a("tab2", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'impacts')", style = "color: grey; text-decoration: none;"),
      
      # tags$a("Country", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'about')", style = "color: grey; text-decoration: none;"),
      # tags$a("Country Capital", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'popcal')", style = "color: grey; text-decoration: none;"),
      # 
      # tags$a("GBD", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'facts')", style = "color: grey; text-decoration: none;"),
      # tags$a("Factsheet", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'reports')", style = "color: grey; text-decoration: none;"),
      # tags$a("About", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'news')", style = "color: grey; text-decoration: none;")
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
      tags$a("Country Overview", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'index')"),
      tags$a("tab2", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'impacts')"),
      
      # tags$a("Country", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'about')"),
      # tags$a("Country Capital", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'popcal')"),
      # 
      # tags$a("GBD", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'facts')"),
      # tags$a("Factsheet", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'reports')"),
      # tags$a("About", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'news')")
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
     fluidRow(
       # selectInput("country", "Select Country",
       #             choices = unique(data_clean$country_name)),
       
       
       column(3,
              pickerInput("country", 
                          choices = unique(final_data$country)) , multiple = TRUE,
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
              selected = unique(data_clean$country)[1]
       ),
       
       column(3,
              pickerInput("year_range",
                          choices = sort(unique(final_data$year)), multiple = TRUE,
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
                          selected = unique(data_clean$year)
              )
       ),
       
     ),
     
     # ==================== PROFESSIONAL & COLORFUL VALUE BOXES ====================
     fluidRow(
       id = "value_boxes",
       style = "margin-bottom: 30px;",
       
       column(3,
              div(class = "value-box", 
                  style = "background: linear-gradient(135deg, #800000, #b22222);",
                  tags$i(class = "bi bi-globe-americas value-box-icon"),
                  div(class = "value-box-title", "Countries Covered"),
                  div(class = "value-box-value", textOutput("vb_countries", inline = TRUE))
              )
       ),
       
       column(3,
              div(class = "value-box", 
                  style = "background: linear-gradient(135deg, #D4AF37, #f0c040); color: #1a1a1a;",
                  tags$i(class = "bi bi-calendar-range value-box-icon"),
                  div(class = "value-box-title", "Awardees Supported"),
                  div(class = "value-box-value", textOutput("vb_years", inline = TRUE))
              )
       ),
       
       column(3,
              div(class = "value-box", 
                  style = "background: linear-gradient(135deg, #006666, #00b3b3);",
                  tags$i(class = "bi bi-cloud-haze2-fill value-box-icon"),
                  div(class = "value-box-title", "Total Monitors Installed"),
                  div(class = "value-box-value", 
                      textOutput("vb_avg_pm25", inline = TRUE), 
                      tags$small(" Units", style = "font-size: 16px; opacity: 0.9;"))
              )
       ),
       
       column(3,
              div(class = "value-box", 
                  style = "background: linear-gradient(135deg, #2E8B57, #3CB371);",
                  tags$i(class = "bi bi-heart-pulse value-box-icon"),
                  div(class = "value-box-title", "Lives at Risk"),
                  div(class = "value-box-value", textOutput("vb_health_impact", inline = TRUE))
              )
       )
     ),
     
      # Filters inside Index Section
    
     br(),
     
     fluidRow(
       
       
       column(
         6,
         wellPanel(
          
             leafletOutput("map", height = "440px")
           
         )
       ),
       column(6,
              wellPanel(
                tabsetPanel(
                  type = "tabs",  # or "pills"
                  
                  tabPanel("Annual PM2.5 (μg/m³)",
                           highchartOutput("line_pm")
                  ),
                  tabPanel("Monthly PM2.5 (μg/m³)",
                           highchartOutput("monthly_pm25")
                  )
                )
              )
       )
     ),

     
     br(),
     
     
     # fluidRow(
     #   column(
     #     6,
     #     wellPanel(
     # 
     #       highchartOutput("monthly_pm25", height = "440px")
     # 
     #     )
     #   )),
     
     
    )),
    
    # Other hidden sections
    


                   
    hidden(tags$div(id = "impacts_section", 

                    br()

                    
                    
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
  
# ui <- secure_app(ui,
#                  fab_button( position =  "bottom-right", label = "Logout"),
#                  tags_top =
#                    tags$div(
#                     # tags$h4("AQLI Dashboard", style = "align:center"),
#                      tags$img(
#                        src = "epic-aqli-logo.png", width = 400
#                      )
#                    ),
#                  #background  = "linear-gradient( rgba(99, 203, 255,0), rgba(99, 203, 255,1)),url('https://i.ibb.co/CWcsc4m/brlps-wall.png');"
#                  #background  = "linear-gradient( rgba(99, 203, 255,0), rgba(99, 203, 255,1)),url('https://s3.gifyu.com/images/brlps_tattva_new_gif.gif');"
#                                              
#                  # style = sprintf(backgroundImageCSS,  "https://images.plot.ly/language-icons/api-home/r-logo.png")
#                  background = "
#                             radial-gradient(circle at top left, #002D72, #F26B38);
#                             background-size: cover;
#                           ",
#                                          
#                  tags_bottom = tags$div(
#                    tags$p(
#                      "For any assistance, please  contact ",
#                      tags$a(
#                        href = "mailto:guptap@uchicago.edu?Subject=AQLI%20Dashboard",
#                        target="_top", "administrator"
#                      )
#                    )
#                  )
#                  
#                  
#                  
# )

