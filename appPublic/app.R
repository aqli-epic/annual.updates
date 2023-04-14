# setting global options
options(shiny.maxRequestSize = 1200*1024^2)

# download libraries
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(tidyr)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(bslib)
library(viridis)
library(DT)


# source data
# source("aqli.data.explorer.helper.script.R")

# loading the .RData file that contain all required data objects
# save(list = ls(all = TRUE), file= "./appPublic/all.RData")
load("all.RData", .GlobalEnv)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Decrease the font size of the tab text */
      .nav-tabs > li > a {
        font-size: 16px;
        width: 135px;
        text-align: center;
      }
    "))
  ),
  theme = bslib::bs_theme(version = 4, bootswatch = "litera"),
  title = "AQLI Data Explorer",
  titlePanel(
    fluidRow(
      column(width = 5, img(src = "aqli-epic-data-exp-logo.png", height = "100px"))
      # column(width = 10, h2("AQLI Data Explorer", style = "margin-top: -5px;"), offset = 1)
    )
  ),
  shiny::tags$br(),
  tabsetPanel(
    tabPanel("Raw Data Exploration",
             shiny::tags$br(),
             fluidRow(
               column(3, selectizeInput("continent", "Continent", choices = c("World", unique(gadm2_aqli_2021$continent)))),
               column(2, selectizeInput("country", "Country", choices = NULL)),
               column(2, selectizeInput("state", "State", choices = NULL)),
               column(2, selectizeInput("district", "District", choices = NULL)),
               column(2, selectizeInput("years", "Year", choices = latest_year:first_year, multiple = TRUE, selected = latest_year))
             ),
             shiny::tags$br(),
             shiny::tags$hr(),
             fluidRow(
               column(10),
               column(2, downloadButton("downloadData", "Download CSV"))
             ),
             shiny::tags$br(),
             shinycssloaders::withSpinner(DT::dataTableOutput("table"))
    ),
    tabPanel("Regional Stats",
             shiny::tags$br(),
             fluidRow(
               column(2, sliderInput("top", "Top", min = 1, max = 50, value = 10)),
               column(2, selectInput("most", "Most", choices = c("Populated", "Polluted"), selected = "Populated")),
               column(2, selectInput("region_type", "Region Type", choices = c("State(s)", "District(s)"), selected = "State(s)")),
               column(1, textOutput("inText")),
               column(2, selectizeInput("country2", "Country", choices = unique(gadm2_aqli_2021$country))),
               column(1, textOutput("inText2")),
               column(2, selectInput("year_range", "Year", choices = latest_year:first_year))
             ),
             fluidRow(
               shiny::tags$hr(),
               column(4),
               column(4),
               column(4, selectInput("standard_type_1", "Relative to", choices = c("WHO PM2.5 Standard", "National PM2.5 Standard"))),
               shiny::tags$br(),
               shiny::tags$hr(),
               shiny::tags$br()
             ),
             fluidRow(
               column(6,  shinycssloaders::withSpinner(shiny::plotOutput("plot"))),
               column(6,  shinycssloaders::withSpinner(shiny::plotOutput("plot_lyl")))
             ),
             shiny::tags$br(),
             shiny::tags$br(),
             shiny::tags$hr(),
             fluidRow(
               column(10),
               column(2, downloadButton("downloadData2", "Download CSV"))
             ),
             shiny::tags$br(),
             fluidRow(
               column(12,  shinycssloaders::withSpinner(DT::dataTableOutput("table2")))
             )
    ),
    tabPanel("Trendlines",
             shiny::tags$br(),
             fluidRow(
               column(4, selectizeInput("country3", "Country", choices =c(unique(gadm2_aqli_2021$country)))),
               column(4, selectizeInput("state3", "State", choices = NULL)),
               column(4, selectizeInput("district3", "District", choices = NULL)),
               shiny::tags$hr()
             ),
             shinycssloaders::withSpinner(plotOutput("plot_trendlines"))
    ),
    tabPanel("Pollution/LYL Maps (coming soon!)",
             shiny::tags$br(),
             shiny::tags$br(),
             fluidRow(
               column(3, selectizeInput("country4", "Country", choices = unique(gadm2_aqli_2021$country))),
               column(3, selectInput("graph_type", "Type", choices = c("Pollution", "LYL"), selected = "Pollution")),
               column(3,
                shiny::tags$br(),
                actionButton("generateMap", "Click to Generate Map"))
             ),
             # plotly::plotlyOutput("map"),
             shinycssloaders::withSpinner(plotOutput("map"))
    ),
    tabPanel("Distributions",
             shiny::tags$br(),
             fluidRow(
               column(2, selectizeInput("continent5", "Continent", choices = c("World", unique(gadm2_aqli_2021$continent)))),
               column(2, selectizeInput("country5", "Country", choices = NULL)),
               column(2, selectizeInput("state5", "State", choices = NULL)),
               column(2, selectizeInput("district5", "District", choices = NULL)),
               column(2, selectInput("dist_type5", "Distribution Type", choices = c("PM2.5 Pollution", "LYL rel to WHO standard",
                                                                                    "LYL rel to National standard"))),
               column(2, selectInput("year5", "Year", choices = c(latest_year:first_year)))

             ),
             shiny::tags$br(),
             shiny::tags$hr(),
             shinycssloaders::withSpinner(plotOutput("distribution_plot_5"))
             ),
    tabPanel("GADM Level Summary",
             shiny::tags$br(),
             fluidRow(
               column(2, selectizeInput("summary_level6", "Summary Level", choices = c("Continent", "Country", "State", "District"))),
               column(2, selectizeInput("continent6", "Continent", NULL)),
               column(2, selectizeInput("country6", "Country", choices = NULL)),
               column(2, selectizeInput("state6", "State", choices = NULL)),
               column(2, selectizeInput("district6", "District", choices = NULL)),
               column(2, selectizeInput("years6", "Year", choices = latest_year:first_year, multiple = TRUE, selected = latest_year))
             ),
             shiny::tags$br(),
             shiny::tags$hr(),
             fluidRow(
               column(3,
                      actionButton("generate_summary_table6", "Click to Generate Summary Data")),
               column(7),
               column(2, downloadButton("downloadData6", "Download CSV"))
             ),
             shiny::tags$br(),
            shinycssloaders::withSpinner(DT::dataTableOutput("table6"))
    ),
    tabPanel("Compare Regions",
             shiny::tags$br(),
             fluidRow(
               column(2, selectizeInput("comparison_level7", "Comparison Level", choices = c("Continent", "Country", "State", "District"))),
               column(2, selectizeInput("continent7", "Continent", NULL, multiple = TRUE)),
               column(2, selectizeInput("country7", "Country", choices = NULL, multiple = TRUE)),
               column(2, selectizeInput("state7", "State", choices = NULL, multiple = TRUE)),
               column(2, selectizeInput("district7", "District", choices = NULL, multiple = TRUE)),
               column(2, selectizeInput("years7", "Year", choices = latest_year:first_year, multiple = TRUE, selected = latest_year))
             ),
             shiny::tags$br(),
             fluidRow(
               column(5),
               column(4,
                      actionButton("compare_regions_button7", "Click to Compare Regions")),
               column(3)
             ),
             shiny::tags$hr(),
             fluidRow(
               column(6,
                      shiny::tags$br(),
                      shinycssloaders::withSpinner(shiny::plotOutput("plot_7_1"))),
               column(6,
                      shiny::tags$br(),
                      shinycssloaders::withSpinner(shiny::plotOutput("plot_7_2")))
             ),
             shiny::tags$br(),
             shiny::tags$br(),
             shiny::tags$hr(),
             fluidRow(
               column(10),
               column(2, downloadButton("downloadData7", "Download CSV"))
             ),
             shiny::tags$br(),
             fluidRow(
               column(12,  shinycssloaders::withSpinner(DT::dataTableOutput("table7")))
             )
             ),
    tabPanel("About AQLI",
             shiny::tags$br(),
             shiny::tags$h6("The Air Quality Life Index, or AQLI, converts air pollution concentrations into their impact on life expectancy. From this, the public and policymakers alike can determine the benefits of air pollution policies in
             perhaps the most important measure that exists: longer lives."),
             shiny::tags$br(),
             shiny::tags$p("The AQLI is rooted in peer-reviewed research by an international team of scholars, including Michael Greenstone from the University of Chicago, which for the first time quantified the causal relationship between long-term human exposure to air pollution and life expectancy. The Index combines this research with hyper-localized, global particulate matter measurements. This unique approach makes the AQLI the first pollution index to show what the threat of air pollution means to a person’s life anywhere in the world. It can also illustrate the gains in life expectancy that could be achieved by reducing particulate pollution concentrations to meet the World Health
                            Organization (WHO) guidelines, existing national air quality standards, or user-defined levels."),
             shiny::tags$br(),
             shiny::tags$p("The video below touches on some key aspects of the AQLI interactive pollution and life years lost map tool:"),
             shiny::tags$br(),
             shiny::tags$div(
               style = "text-align:center;",
               # tags$iframe(width = "100%", height = "315",
               #             src = "https://www.youtube.com/watch?v=_z-_D9vxCWE"),
               HTML('<iframe width="560" height="315" src="aqli_screen_capture_video.mp4" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
               shiny::tags$br()
             ),
             shiny::tags$br(),
             "To explore the AQLI interactive tool yourself, visit the AQLI website ", shiny::tags$a("here.", href = "https://aqli.epic.uchicago.edu/the-index/"), "If you are curious
             to learn more about the methodology underlying the tool, click ", shiny::tags$a("here.", href = "https://aqli.epic.uchicago.edu/about/methodology/"),
             shiny::tags$br(),
             shiny::tags$br(),
             shiny::tags$br()

    )
  )
)

# Define server
server <- function(input, output, session) {

#> Raw data exploration tab---------------------------------------------------

# continent drop down observer
shiny::observeEvent(input$continent, {
  shiny::updateSelectizeInput(session, "country", choices = c("all", gadm2_aqli_2021 %>% filter(continent == input$continent) %>% pull(country) %>% unique() ))

})

# country drop down observer
shiny::observeEvent(input$country, {
    shiny::updateSelectizeInput(session, "state", choices = c("all", gadm2_aqli_2021 %>% filter(country == input$country) %>% pull(name_1) %>% unique()))

  })

# state drop down observer
shiny::observeEvent(input$state, {
  shiny::updateSelectizeInput(session, "district", choices = c("all", gadm2_aqli_2021 %>% filter(name_1 == input$state) %>% pull(name_2) %>% unique() ))

})


# filtered data, given the dropdowns

# reactive pollution column name
pol_col_0 <- shiny::reactive({
  stringr::str_c("pm", input$years)
})

# reactive llpp_who_col name
llpp_who_col_0 <- shiny::reactive({
  stringr::str_c("llpp_who_", input$years)
})

# reactive llpp_nat_col name
llpp_nat_col_0 <- shiny::reactive({
  stringr::str_c("llpp_nat_", input$years)
})


filteredData <- shiny::reactive({
    if(input$continent == "World"){
      gadm2_aqli_2021 %>%
        dplyr::select(objectid_gadm2:natstandard, dplyr::all_of(c(pol_col_0(), llpp_who_col_0(), llpp_nat_col_0())))

    } else {
      if(input$country == "all"){
        gadm2_aqli_2021 %>%
          dplyr::filter(continent == input$continent) %>%
          dplyr::select(objectid_gadm2:natstandard, dplyr::all_of(c(pol_col_0(), llpp_who_col_0(), llpp_nat_col_0())))
      } else{
        if(input$state == "all"){
          gadm2_aqli_2021 %>%
            dplyr::filter(continent == input$continent, country == input$country) %>%
            dplyr::select(objectid_gadm2:natstandard, dplyr::all_of(c(pol_col_0(), llpp_who_col_0(), llpp_nat_col_0())))
        } else{
          if(input$district == "all"){
            gadm2_aqli_2021 %>%
              dplyr::filter(continent == input$continent, country == input$country, name_1 == input$state) %>%
              dplyr::select(objectid_gadm2:natstandard, dplyr::all_of(c(pol_col_0(), llpp_who_col_0(), llpp_nat_col_0())))
          } else {
            gadm2_aqli_2021 %>%
              dplyr::filter(continent == input$continent, country == input$country, name_1 == input$state, name_2 == input$district) %>%
              dplyr::select(objectid_gadm2:natstandard, dplyr::all_of(c(pol_col_0(), llpp_who_col_0(), llpp_nat_col_0())))
          }

        }
      }
    }

  })

# render the filtered dataset
  output$table <- DT::renderDataTable(
    filteredData()
  )

# download button of the filtered dataset
  output$downloadData <- downloadHandler(
    filename = "filtered_data.csv",
    content = function(file) {
      write.csv(filteredData(), file)
    }
  )


#> Regional Stats tab----------------------------------------------------

  # reactive pollution column name
  pol_col <- shiny::reactive({
    stringr::str_c("pm", input$year_range)
  })

  # reactive llpp_who_col name
  llpp_who_col <- shiny::reactive({
    stringr::str_c("llpp_who_", input$year_range)
  })

  # reactive llpp_nat_col name
  llpp_nat_col <- shiny::reactive({
    stringr::str_c("llpp_nat_", input$year_range)
  })

  # create the filtered dataset for the regional stats tab
  filteredData2 <- shiny::reactive({
    if(input$region_type == "State(s)"){
      if(input$most == "Populated"){
        gadm1_aqli_2021 %>%
          dplyr::filter(country == input$country2) %>%
          dplyr::select(country, name_1, population, natstandard, !!as.symbol(pol_col()), !!as.symbol(llpp_who_col()), !!as.symbol(llpp_nat_col())) %>%
          dplyr::slice_max(population, n = input$top)

      } else if (input$most == "Polluted"){
        gadm1_aqli_2021 %>%
          dplyr::filter(country == input$country2) %>%
          dplyr::select(country, name_1, population, natstandard, !!as.symbol(pol_col()), !!as.symbol(llpp_who_col()), !!as.symbol(llpp_nat_col())) %>%
          dplyr::slice_max(!!as.symbol(pol_col()), n = input$top)
      }

    } else if (input$region_type == "District(s)") {
        if(input$most == "Populated"){
          gadm2_aqli_2021 %>%
            dplyr::filter(country == input$country2) %>%
            dplyr::select(country, name_1, name_2, population, natstandard, !!as.symbol(pol_col()), !!as.symbol(llpp_who_col()), !!as.symbol(llpp_nat_col())) %>%
            dplyr::slice_max(population, n = input$top)

        } else if (input$most == "Polluted"){
          gadm2_aqli_2021 %>%
            dplyr::filter(country == input$country2) %>%
            dplyr::select(country, name_1, name_2, population, natstandard, !!as.symbol(pol_col()), !!as.symbol(llpp_who_col()), !!as.symbol(llpp_nat_col())) %>%
            dplyr::slice_max(!!as.symbol(pol_col()), n = input$top)
        }
    }
  })

  # intermediate text outputs
  output$inText <- renderText("in")
  output$inText2 <- renderText("in")

  # output the filtered data for the regional stats tab
  output$table2 <- DT::renderDataTable(
    filteredData2()
  )

  # plot based on selected values of the dropdown columns in the regional stats tab (pollution graph)
  output$plot <- shiny::renderPlot({
    if(input$region_type == "State(s)"){
      if(input$most == "Polluted"){
        filteredData2() %>%
          ggplot2::ggplot(mapping = aes(x = forcats::fct_reorder(name_1, !!as.symbol(pol_col())), y = !!as.symbol(pol_col()))) +
          ggplot2::geom_col(fill = "cornflowerblue") +
          ggplot2::coord_flip() +
          ggthemes::theme_hc() +
          ggplot2::labs(x = "State", y = "Annual Average PM2.5 concentration (in µg/m³)",
               title = str_c("Pollution Graph: Top ", input$top, " most Polluted States in ", input$country2, " in ", input$year_range))



      } else if (input$most == "Populated"){
      filteredData2() %>%
          ggplot2::ggplot(mapping = aes(x = forcats::fct_reorder(name_1, !!as.symbol(pol_col())), y = !!as.symbol(pol_col()))) +
          ggplot2::geom_col(fill = "cornflowerblue") +
          ggplot2::coord_flip() +
          ggthemes::theme_hc() +
          ggplot2::labs(x = "State", y = "Annual Average PM2.5 concentration (in µg/m³)",
               title = str_c("Pollution Graph: Top ", input$top, " most Populated States in ", input$country2, " in ", input$year_range))



      }
    } else if (input$region_type == "District(s)"){
      if(input$most == "Polluted"){
        filteredData2() %>%
          ggplot2::ggplot(mapping = aes(x = forcats::fct_reorder(name_2, !!as.symbol(pol_col())), y = !!as.symbol(pol_col()))) +
          ggplot2::geom_col(fill = "cornflowerblue") +
          ggplot2::coord_flip() +
          ggthemes::theme_hc() +
          ggplot2::labs(x = "District", y = "Annual Average PM2.5 concentration (in µg/m³)",
               title = str_c("Pollution Graph: Top ", input$top, " most Polluted Districts in ", input$country2, " in ", input$year_range))



      } else if (input$most == "Populated"){
    filteredData2() %>%
          ggplot2::ggplot(mapping = aes(x = forcats::fct_reorder(name_2, !!as.symbol(pol_col())), y = !!as.symbol(pol_col()))) +
          ggplot2::geom_col(fill = "cornflowerblue") +
          ggplot2::coord_flip() +
          ggthemes::theme_hc() +
          ggplot2::labs(x = "District", y = "Annual Average PM2.5 concentration (in µg/m³)" ,
               title = str_c("Pollution Graph: Top ", input$top, " most Populated Districts in ", input$country2, " in ", input$year_range))



      }
    }
  })

  # plot 2 based on selected values of the dropdown columns in the regional stats tab (lyl graph)
  output$plot_lyl <- renderPlot({
    if(input$region_type == "State(s)"){
      if(input$most == "Polluted"){
        if(input$standard_type_1 == "WHO PM2.5 Standard"){
         filteredData2() %>%
            ggplot2::ggplot(mapping = aes(x = forcats::fct_reorder(name_1, !!as.symbol(llpp_who_col())), y = !!as.symbol(llpp_who_col()))) +
            ggplot2::geom_col(fill = "darkred") +
            ggplot2::coord_flip() +
            ggthemes::theme_hc() +
            ggplot2::labs(x = "State", y = "LYL relative to WHO PM2.5 Standard",
                 title = str_c("LYL Graph: Top ", input$top, " most Polluted States in ", input$country2, " in ", input$year_range))



        } else if (input$standard_type_1 == "National PM2.5 Standard") {
         filteredData2() %>%
            ggplot2::ggplot(mapping = aes(x = forcats::fct_reorder(name_1, !!as.symbol(llpp_nat_col())), y = !!as.symbol(llpp_nat_col()))) +
            ggplot2::geom_col(fill = "darkred") +
            ggplot2::coord_flip() +
            ggthemes::theme_hc() +
            ggplot2::labs(x = "State", y = "LYL relative to National PM2.5 Standard",
                 title = str_c("LYL Graph: Top ", input$top, " most Polluted States in ", input$country2, " in ", input$year_range))


        }
      } else if (input$most == "Populated"){
        if(input$standard_type_1 == "WHO PM2.5 Standard"){
         filteredData2() %>%
            ggplot2::ggplot(mapping = aes(x = forcats::fct_reorder(name_1, !!as.symbol(llpp_who_col())), y = !!as.symbol(llpp_who_col()))) +
            ggplot2::geom_col(fill = "darkred") +
            ggplot2::coord_flip() +
            ggthemes::theme_hc() +
            ggplot2::labs(x = "State", y = "LYL relative to WHO PM2.5 Standard",
                 title = str_c("LYL Graph: Top ", input$top, " most Polluted States in ", input$country2, " in ", input$year_range))



         } else if (input$standard_type_1 == "National PM2.5 Standard") {
          filteredData2() %>%
            ggplot2::ggplot(mapping = aes(x = forcats::fct_reorder(name_1, !!as.symbol(llpp_nat_col())), y = !!as.symbol(llpp_nat_col()))) +
            ggplot2::geom_col(fill = "darkred") +
            ggplot2::coord_flip() +
            ggthemes::theme_hc() +
            ggplot2::labs(x = "State", y = "LYL relative to National PM2.5 Standard",
                 title = str_c("LYL Graph: Top ", input$top, " most Polluted States in ", input$country2, " in ", input$year_range))



        }
      }
    } else if (input$region_type == "District(s)"){
      if(input$most == "Polluted"){
        if(input$standard_type_1 == "WHO PM2.5 Standard"){
          filteredData2() %>%
            ggplot2::ggplot(mapping = aes(x = forcats::fct_reorder(name_2, !!as.symbol(llpp_who_col())), y = !!as.symbol(llpp_who_col()))) +
            ggplot2::geom_col(fill = "darkred") +
            ggplot2::coord_flip() +
            ggthemes::theme_hc() +
            ggplot2::labs(x = "District", y = "LYL relative to WHO PM2.5 Standard",
                 title = str_c("LYL Graph: Top ", input$top, " most Polluted States in ", input$country2, " in ", input$year_range))



        } else if (input$standard_type_1 == "National PM2.5 Standard") {
          filteredData2() %>%
            ggplot2::ggplot(mapping = aes(x = forcats::fct_reorder(name_2, !!as.symbol(llpp_nat_col())), y = !!as.symbol(llpp_nat_col()))) +
            ggplot2::geom_col(fill = "darkred") +
            ggplot2::coord_flip() +
            ggthemes::theme_hc() +
            ggplot2::labs(x = "District", y = "LYL relative to National PM2.5 Standard",
                 title = str_c("LYL Graph: Top ", input$top, " most Polluted States in ", input$country2, " in ", input$year_range))


        }

      } else if (input$most == "Populated"){
        if(input$standard_type_1 == "WHO PM2.5 Standard"){
          filteredData2() %>%
            ggplot2::ggplot(mapping = aes(x = forcats::fct_reorder(name_2, !!as.symbol(llpp_who_col())), y = !!as.symbol(llpp_who_col()))) +
            ggplot2::geom_col(fill = "darkred") +
            ggplot2::coord_flip() +
            ggthemes::theme_hc() +
            ggplot2::labs(x = "District", y = "LYL relative to WHO PM2.5 Standard",
                 title = str_c("LYL Graph: Top ", input$top, " most Polluted States in ", input$country2, " in ", input$year_range))



        } else if (input$standard_type_1 == "National PM2.5 Standard") {
          filteredData2() %>%
            ggplot2::ggplot(mapping = aes(x = forcats::fct_reorder(name_2, !!as.symbol(llpp_nat_col())), y = !!as.symbol(llpp_nat_col()))) +
            ggplot2::geom_col(fill = "darkred") +
            ggplot2::coord_flip() +
            ggthemes::theme_hc() +
            ggplot2::labs(x = "District", y = "LYL relative to National PM2.5 Standard",
                 title = str_c("LYL Graph: Top ", input$top, " most Polluted States in ", input$country2, " in ", input$year_range))



        }
      }
    }
  })

# download button in regional stats tab
output$downloadData2 <- downloadHandler(
    filename = "filtered_data2.csv",
    content = function(file) {
      write.csv(filteredData2(), file)
    }
  )

# Trendlines tab-----------------------------------------------------------------

# continent drop down observer
shiny::observeEvent(input$country3, {
  shiny::updateSelectizeInput(session, "state3", choices = c("all", gadm2_aqli_2021 %>% filter(country == input$country3) %>% pull(name_1) %>% unique() ))

})

# country drop down observer
shiny::observeEvent(input$state3, {
  shiny::updateSelectizeInput(session, "district3", choices = c("all", gadm2_aqli_2021 %>% filter(name_1 == input$state3) %>% pull(name_2) %>% unique()))

})

output$plot_trendlines <- shiny::renderPlot({

  if(input$state3 == "all"){
    if(input$district3 == "all"){
      trendlines_aqli(gadm2_aqli_2021, level = "country", country_name = input$country3, start_year = 1998, end_year = 2021)
      # return(plotly::ggplotly(plt))

    }

  } else {
    if(input$district3 == "all"){
       trendlines_aqli(gadm2_aqli_2021, level = "state", country_name = input$country3, state_name = input$state3, start_year = 1998, end_year = 2021)
      # return(plotly::ggplotly(plt))
      } else {
      trendlines_aqli(gadm2_aqli_2021, level = "district", country_name = input$country3, state_name = input$state3, district_name = input$district3, start_year = 1998, end_year = 2021)
      # return(plotly::ggplotly(plt))
    }
  }


})


# Pollution/LYL Maps--------------------------------------------------------------

output$map <- shiny::renderPlot(ggplot() + theme(plot.background = element_rect(fill = "white")))

  observeEvent(input$generateMap, {

    # filteredData3 <- gadm2_aqli_2021_shp %>%
    #   dplyr::filter(country == input$country4)
    #
    # if (input$graph_type == "Pollution") {
    #   map <- filteredData3 %>%
    #     ggplot2::ggplot() +
    #     ggplot2::geom_sf(mapping = aes(fill = pm2021)) +
    #     ggthemes::theme_tufte() +
    #     viridis::scale_fill_viridis(option = "blues", direction = -1) +
    #     ggplot2::theme(axis.text = ggplot2::element_blank(),
    #                    axis.ticks = ggplot2::element_blank()) +
    #     ggplot2::ggtitle(stringr::str_c("PM2.5 pollution in ", input$country4, " in ", "2021"))
    #
    # } else {
    #   map <- filteredData3 %>%
    #     ggplot2::ggplot() +
    #     ggplot2::geom_sf(mapping = aes(fill = llw2021)) +
    #     ggthemes::theme_tufte() +
    #     viridis::scale_fill_viridis(option = "rocket", direction = -1) +
    #     ggplot2::theme(axis.text = ggplot2::element_blank(),
    #                    axis.ticks = ggplot2::element_blank()) +
    #     ggplot2::ggtitle(stringr::str_c("Life Years Lost in ", input$country4, " in ", "2021"))
    #
    #
    # }
    #
    # # output$map <- plotly::renderPlotly(plotly::ggplotly(map))
    # output$map <- shiny::renderPlot(map)

  })


  # Distributions tab--------------------------------------------------------------------

  # continent drop down observer for the distributions tab
  shiny::observeEvent(input$continent5, {
    shiny::updateSelectizeInput(session, "country5", choices = c("all", gadm2_aqli_2021 %>% filter(continent == input$continent5) %>% pull(country) %>% unique() ))

  })

  # country drop down observer for the distributions tab
  shiny::observeEvent(input$country5, {
    shiny::updateSelectizeInput(session, "state5", choices = c("all", gadm2_aqli_2021 %>% filter(country == input$country5) %>% pull(name_1) %>% unique()))

  })

  # state drop down observer for the distributions tab
  shiny::observeEvent(input$state5, {
    shiny::updateSelectizeInput(session, "district5", choices = c("all", gadm2_aqli_2021 %>% filter(name_1 == input$state5) %>% pull(name_2) %>% unique() ))

  })


  # filtered data, given the dropdowns for the distributions plot
  filteredData5 <- shiny::reactive({
    if(input$continent5 == "World"){
      gadm2_aqli_2021
    } else {
      if(input$country5 == "all"){
        gadm2_aqli_2021 %>%
          dplyr::filter(continent == input$continent5)
      } else{
        if(input$state5 == "all"){
          gadm2_aqli_2021 %>%
            dplyr::filter(continent == input$continent5, country == input$country5)
        } else{
          if(input$district5 == "all"){
            gadm2_aqli_2021 %>%
              dplyr::filter(continent == input$continent5, country == input$country5, name_1 == input$state5)
          } else {
            gadm2_aqli_2021 %>%
              dplyr::filter(continent == input$continent5, country == input$country5, name_1 == input$state5, name_2 == input$district5)
          }

        }
      }
    }

  })

# plot the distribution given the selected options

output$distribution_plot_5 <- shiny::renderPlot({

  # reactive pollution column name
  pol_col_5 <- shiny::reactive({
    stringr::str_c("pm", input$year5)
  })

  # reactive llpp_who_col name
  llpp_who_col_5 <- shiny::reactive({
    stringr::str_c("llpp_who_", input$year5)
  })

  # reactive llpp_nat_col name
  llpp_nat_col_5 <- shiny::reactive({
    stringr::str_c("llpp_nat_", input$year5)
  })

  if(input$dist_type5 == "PM2.5 Pollution"){
    filteredData5() %>%
      ggplot2::ggplot() +
      ggplot2::geom_histogram(mapping = aes(x = !!as.symbol(pol_col_5())), color = "white", fill = "cornflowerblue") +
      ggthemes::theme_hc() +
      ggplot2::scale_y_log10()


  } else if (input$dist_type5 == "LYL rel to WHO standard") {
    filteredData5() %>%
      ggplot2::ggplot() +
      ggplot2::geom_histogram(mapping = aes(x = !!as.symbol(llpp_who_col_5())), color = "white", fill = "darkred") +
      ggthemes::theme_hc() +
      ggplot2::scale_y_log10()


  } else if (input$dist_type5 == "LYL rel to National standard"){
    filteredData5() %>%
      ggplot2::ggplot() +
      ggplot2::geom_histogram(mapping = aes(x = !!as.symbol(llpp_nat_col_5())), color = "white", fill = "darkred") +
      ggthemes::theme_hc() +
      ggplot2::scale_y_log10()
  }

})

#> GADM level summary tab--------------------------------------------------------------

 # summary level drop down observer
 shiny::observeEvent(input$summary_level6, {
   if(input$summary_level6 == "Continent"){
   shiny::updateSelectizeInput(session, "continent6", choices = c("all", gadm2_aqli_2021$continent %>% unique()))
     shiny::updateSelectizeInput(session, "country6", choices = c("all"))
   shiny::updateSelectizeInput(session, "state6", choices = c("all"))
   shiny::updateSelectizeInput(session, "district6", choices = c("all"))
   } else if (input$summary_level6 == "Country"){
     shiny::updateSelectizeInput(session, "continent6", choices = c("all", unique(gadm2_aqli_2021$continent)))
     shiny::updateSelectizeInput(session, "country6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6) %>% dplyr::pull(country) %>% unique() ))
     shiny::updateSelectizeInput(session, "state6", choices = c("all"))
     shiny::updateSelectizeInput(session, "district6", choices = c("all"))
   } else if (input$summary_level6 == "State"){
     shiny::updateSelectizeInput(session, "continent6", choices = c("all", unique(gadm2_aqli_2021$continent)))
   shiny::updateSelectizeInput(session, "country6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6) %>% dplyr::pull(country) %>% unique() ))
     shiny::updateSelectizeInput(session, "state6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6, country == input$country6) %>% dplyr::pull(name_1) %>% unique()))
     shiny::updateSelectizeInput(session, "district6", choices = c("all"))
   } else if (input$summary_level6 == "District"){
   shiny::updateSelectizeInput(session, "continent6", choices = c("all", unique(gadm2_aqli_2021$continent)))
     shiny::updateSelectizeInput(session, "country6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6) %>% dplyr::pull(country) %>% unique() ))
     shiny::updateSelectizeInput(session, "state6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6, country == input$country6) %>% dplyr::pull(name_1) %>% unique()))
     shiny::updateSelectizeInput(session, "district6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6, country == input$country6, name_1 == input$state6) %>% dplyr::pull(name_2) %>% unique()))
   }
 })


#  continent drop down observer
 shiny::observeEvent(input$continent6, {
   if(input$summary_level6 == "Continent"){
     shiny::updateSelectizeInput(session, "country6", choices = c("all"))
     shiny::updateSelectizeInput(session, "state6", choices = c("all"))
     shiny::updateSelectizeInput(session, "district6", choices = c("all"))

   } else if (input$summary_level6 == "Country"){
     shiny::updateSelectizeInput(session, "country6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6) %>% dplyr::pull(country) %>% unique()))
     shiny::updateSelectizeInput(session, "state6", choices = c("all"))
     shiny::updateSelectizeInput(session, "district6", choices = c("all"))

   } else if (input$summary_level6 == "State"){
     shiny::updateSelectizeInput(session, "country6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6) %>% dplyr::pull(country) %>% unique()))
     shiny::updateSelectizeInput(session, "state6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6, country == input$country6) %>% dplyr::pull(name_1) %>% unique()))
     shiny::updateSelectizeInput(session, "district6", choices = c("all"))

   } else if (input$summary_level6 == "District"){
     shiny::updateSelectizeInput(session, "country6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6) %>% dplyr::pull(country) %>% unique() ))
     shiny::updateSelectizeInput(session, "state6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6, country == input$country6) %>% dplyr::pull(name_1) %>% unique()))
     shiny::updateSelectizeInput(session, "district6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6, country == input$country6, name_1 == input$state6) %>% dplyr::pull(name_2) %>% unique()))
  }

 })

 # country drop down observer
 shiny::observeEvent(input$country6, {

   if(input$summary_level6 == "Country"){
     shiny::updateSelectizeInput(session, "state6", choices = c("all"))
     shiny::updateSelectizeInput(session, "district6", choices = c("all"))

   } else if (input$summary_level6 == "State"){
     shiny::updateSelectizeInput(session, "state6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6, country == input$country6) %>% dplyr::pull(name_1) %>% unique()))
     shiny::updateSelectizeInput(session, "district6", choices = c("all"))

   } else if (input$summary_level6 == "District"){
     shiny::updateSelectizeInput(session, "state6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6, country == input$country6) %>% dplyr::pull(name_1) %>% unique()))
     shiny::updateSelectizeInput(session, "district6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6, country == input$country6, name_1 == input$state6) %>% dplyr::pull(name_2) %>% unique()))

   }
 })

# state drop down observer
 shiny::observeEvent(input$state6, {

   if (input$summary_level6 == "State"){
     shiny::updateSelectizeInput(session, "district6", choices = c("all"))

   } else if (input$summary_level6 == "District"){
     #shiny::updateSelectizeInput(session, "state6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6, country == input$country6) %>% dplyr::pull(name_1) %>% unique()))
     shiny::updateSelectizeInput(session, "district6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6, country == input$country6, name_1 == input$state6) %>% dplyr::pull(name_2) %>% unique()))

   }
 })


# filtered data, given the dropdowns

   filteredData6 <- shiny::eventReactive(input$generate_summary_table6, {

     if(input$summary_level6 == "Continent"){
       serve_summary_data <- gadm_level_summary(gadm2_aqli_2021, c("continent"), input$years6)
       if(input$continent6 == "all"){
         serve_summary_data
       } else{
         serve_summary_data %>%
           dplyr::filter(continent == input$continent6)

       }

     } else if(input$summary_level6 == "Country"){
       serve_summary_data <- gadm_level_summary(gadm2_aqli_2021, c("continent", "country"), input$years6)
       if((input$continent6 == "all") & (input$country6 == "all")){
         serve_summary_data
       } else if ((input$continent6 != "all") & (input$country6 == "all")){
         serve_summary_data %>%
           dplyr::filter(continent == input$continent6)

       } else if ((input$continent6 != "all") & (input$country6 != "all")) {
         serve_summary_data %>%
           dplyr::filter(continent == input$continent6, country == input$country6)

       }

     } else if(input$summary_level6 == "State"){

       serve_summary_data <-  gadm_level_summary(gadm2_aqli_2021, c("continent", "country", "name_1"), input$years6)
       if((input$continent6 == "all") & (input$country6 == "all") & (input$state6 == "all")){

         serve_summary_data
       } else if ((input$continent6 != "all") & (input$country6 == "all") & (input$state6 == "all")){

         serve_summary_data %>%
           dplyr::filter(continent == input$continent6)

       } else if ((input$continent6 != "all") & (input$country6 != "all") & (input$state6 == "all")) {

         serve_summary_data %>%
           dplyr::filter(continent == input$continent6, country == input$country6)

       } else if ((input$continent6 != "all") & (input$country6 != "all") & (input$state6 != "all")) {

         serve_summary_data %>%
           dplyr::filter(continent == input$continent6, country == input$country6, name_1 == input$state6)

       }

     } else if(input$summary_level6 == "District") {

       serve_summary_data <- gadm_level_summary(gadm2_aqli_2021, c("continent", "country", "name_1", "name_2"), input$years6)

       if((input$continent6 == "all") & (input$country6 == "all") & (input$state6 == "all") & (input$district6 == "all")){

         serve_summary_data

       } else if ((input$continent6 != "all") & (input$country6 == "all") & (input$state6 == "all") & (input$district6 == "all")){

         serve_summary_data %>%
           dplyr::filter(continent == input$continent6)

       } else if ((input$continent6 != "all") & (input$country6 != "all") & (input$state6 == "all") & (input$district6 == "all")) {
         serve_summary_data %>%
           dplyr::filter(continent == input$continent6, country == input$country6)

       } else if ((input$continent6 != "all") & (input$country6 != "all") & (input$state6 != "all") & (input$district6 == "all")){
         serve_summary_data %>%
           dplyr::filter(continent == input$continent6, country == input$country6, name_1 == input$state6)

       } else if ((input$continent6 != "all") & (input$country6 != "all") & (input$state6 != "all") & (input$district6 != "all")){
         serve_summary_data %>%
         dplyr::filter(continent == input$continent6, country == input$country6, name_1 == input$state6, name_2 == input$district6)

       }

     }

   })

   # render the filtered dataset
   output$table6 <- DT::renderDataTable(
     if(input$generate_summary_table6 == 0){
       return(NULL)
     } else {
       options = list(extensions = "Buttons")
       filteredData6()
     }
   )

   # download button of the filtered dataset
   output$downloadData6 <- downloadHandler(
     filename = "summary_data.csv",
     content = function(file) {
       write.csv(filteredData6(), file)
     }
   )




#> Compare Regions tab-----------------------------------------------------------------

 # summary level drop down observer
 shiny::observeEvent(input$comparison_level7, {
   if(input$comparison_level7 == "Continent"){
     shiny::updateSelectizeInput(session, "continent7", choices = c("all", gadm2_aqli_2021$continent %>% unique()), selected = "Asia")
     shiny::updateSelectizeInput(session, "country7", choices = c("all"), selected = "India")
     shiny::updateSelectizeInput(session, "state7", choices = c("all"), selected = "NCT of Delhi")
     shiny::updateSelectizeInput(session, "district7", choices = c("all"), selected = "NCT of Delhi")
   } else if (input$comparison_level7 == "Country"){
     shiny::updateSelectizeInput(session, "continent7", choices = c("all", unique(gadm2_aqli_2021$continent)), selected = "Asia")
     shiny::updateSelectizeInput(session, "country7", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent %in% input$continent7) %>% dplyr::pull(country) %>% unique() ), selected = "India")
     shiny::updateSelectizeInput(session, "state7", choices = c("all"), selected = "NCT of Delhi")
     shiny::updateSelectizeInput(session, "district7", choices = c("all"), selected = "NCT of Delhi")
   } else if (input$comparison_level7 == "State"){
     shiny::updateSelectizeInput(session, "continent7", choices = c("all", unique(gadm2_aqli_2021$continent)), selected = "Asia")
     shiny::updateSelectizeInput(session, "country7", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent %in% input$continent7) %>% dplyr::pull(country) %>% unique() ), selected = "India")
     shiny::updateSelectizeInput(session, "state7", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent %in% input$continent7, country %in% input$country7) %>% dplyr::pull(name_1) %>% unique()), selected = "NCT of Delhi")
     shiny::updateSelectizeInput(session, "district7", choices = c("all"), selected = "NCT of Delhi")
   } else if (input$comparison_level7 == "District"){
     shiny::updateSelectizeInput(session, "continent7", choices = c("all", unique(gadm2_aqli_2021$continent)), selected = "Asia")
     shiny::updateSelectizeInput(session, "country7", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent %in% input$continent7) %>% dplyr::pull(country) %>% unique() ), selected = "India")
     shiny::updateSelectizeInput(session, "state7", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent %in% input$continent7, country %in% input$country7) %>% dplyr::pull(name_1) %>% unique()), selected = "NCT of Delhi")
     shiny::updateSelectizeInput(session, "district7", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent %in% input$continent7, country %in% input$country7, name_1 %in% input$state7) %>% dplyr::pull(name_2) %>% unique()), selected = "NCT of Delhi")
   }
 })


 #  continent drop down observer
 shiny::observeEvent(input$continent7, {
   if(input$comparison_level7 == "Continent"){
     shiny::updateSelectizeInput(session, "country7", choices = c("all"), selected = "all")
     shiny::updateSelectizeInput(session, "state7", choices = c("all"), selected = "all")
     shiny::updateSelectizeInput(session, "district7", choices = c("all"), selected = "all")

   } else if (input$comparison_level7 == "Country"){
     shiny::updateSelectizeInput(session, "country7", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent %in% input$continent7) %>% dplyr::pull(country) %>% unique()), selected = "all")
     shiny::updateSelectizeInput(session, "state7", choices = c("all"), selected = "all")
     shiny::updateSelectizeInput(session, "district7", choices = c("all"), selected = "all")

   } else if (input$comparison_level7 == "State"){
     shiny::updateSelectizeInput(session, "country7", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent %in% input$continent7) %>% dplyr::pull(country) %>% unique()), selected = "all")
     shiny::updateSelectizeInput(session, "state7", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent7, country == input$country7) %>% dplyr::pull(name_1) %>% unique()), selected = "all")
     shiny::updateSelectizeInput(session, "district7", choices = c("all"), selected = "all")

   } else if (input$comparison_level7 == "District"){
     shiny::updateSelectizeInput(session, "country7", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent %in% input$continent7) %>% dplyr::pull(country) %>% unique() ), selected = "all")
     shiny::updateSelectizeInput(session, "state7", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent %in% input$continent7, country %in% input$country7) %>% dplyr::pull(name_1) %>% unique()), selected = "all")
     shiny::updateSelectizeInput(session, "district7", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent %in% input$continent7, country %in% input$country7, name_1 %in% input$state7) %>% dplyr::pull(name_2) %>% unique()), selected = "all")
   }

 })

 # country drop down observer
 shiny::observeEvent(input$country7, {

   if(input$comparison_level7 == "Country"){
     shiny::updateSelectizeInput(session, "state7", choices = c("all"), selected = "all")
     shiny::updateSelectizeInput(session, "district7", choices = c("all"), selected = "all")

   } else if (input$comparison_level7 == "State"){
     shiny::updateSelectizeInput(session, "state7", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent %in% input$continent7, country %in% input$country7) %>% dplyr::pull(name_1) %>% unique()), selected = "all")
     shiny::updateSelectizeInput(session, "district7", choices = c("all"), selected = "all")

   } else if (input$comparison_level7 == "District"){
     shiny::updateSelectizeInput(session, "state7", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent %in% input$continent7, country %in% input$country7) %>% dplyr::pull(name_1) %>% unique()), selected = "all")
     shiny::updateSelectizeInput(session, "district7", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent %in% input$continent7, country %in% input$country7, name_1 %in% input$state7) %>% dplyr::pull(name_2) %>% unique()), selected = "all")

   }
 })

 # state drop down observer
 shiny::observeEvent(input$state7, {

   if (input$comparison_level7 == "State"){
     shiny::updateSelectizeInput(session, "district7", choices = c("all"), selected = "all")

   } else if (input$comparison_level7 == "District"){
     #shiny::updateSelectizeInput(session, "state6", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent == input$continent6, country == input$country6) %>% dplyr::pull(name_1) %>% unique()))
     shiny::updateSelectizeInput(session, "district7", choices = c("all", gadm2_aqli_2021 %>% dplyr::filter(continent %in% input$continent7, country %in% input$country7, name_1 %in% input$state7) %>% dplyr::pull(name_2) %>% unique()), selected = "all")

   }
 })

# filtered data given the dropdown values
 filteredData7 <- eventReactive(input$compare_regions_button7, {

   if(input$comparison_level7 == "Continent"){

     if(length(input$continent7) == 0){
       stop("Continent cannot not be empty in a continent level comparison! Please select atleast one continent to proceed.")
     }

     if ((length(input$continent7) > 1) & ("all" %in% input$continent7)){
        stop("If 'all' is selected in a given dropdown, then other options in that dropdown should not be selected. Please try again! ")
     }

     serve_summary_data7 <- gadm_level_summary(gadm2_aqli_2021, c("continent"), input$years7)
     if(sum(input$continent7 == "all") == 1){
        serve_summary_data7
     } else if ((sum(input$continent7 != "all")) == (length(input$continent7))) {
       serve_summary_data7  %>%
         dplyr::filter(continent %in% input$continent7)
     }



   } else if (input$comparison_level7 == "Country"){

     if((length(input$continent7) == 0) | (length(input$country7) == 0)){
       stop("Both Continent and Country values are needed for a Country level comparison! Please select atleast one continent-country pair to proceed.")
     }

     if ((length(input$continent7) > 1) & ("all" %in% input$continent7)){
       stop("If 'all' is selected in a given dropdown, then other options in that dropdown should not be selected. Please try again! ")
     }

     if ((length(input$country7) > 1) & ("all" %in% input$country7)){
       stop("If 'all' is selected in a given dropdown, then other options in that dropdown should not be selected. Please try again! ")
     }

     serve_summary_data7 <- gadm_level_summary(gadm2_aqli_2021, c("continent", "country"), input$years7)
      if((sum(input$continent7 == "all") == 1) & (sum(input$country7 == "all") == 1)){
        serve_summary_data7
      } else if ((sum(input$continent7 != "all") == length(input$continent7)) & (sum(input$country7 == "all") == 1)){
        serve_summary_data7 %>%
          dplyr::filter(continent %in% input$continent7)
      } else if ((sum(input$continent7 != "all") == length(input$continent7)) & (sum(input$country7 != "all") == length(input$country7))){
        serve_summary_data7 %>%
          dplyr::filter(continent %in% input$continent7) %>%
          dplyr::filter(country %in% input$country7)
      }



   } else if (input$comparison_level7 == "State"){

     if((length(input$continent7) == 0) | (length(input$country7) == 0) | (length(input$state7) == 0)){
       stop("Continent, Country and State values are needed for a State level comparison! Please select atleast one continent-country-state combination to proceed.")
     }

     if ((length(input$continent7) > 1) & ("all" %in% input$continent7)){
       stop("If 'all' is selected in a given dropdown, then other options in that dropdown should not be selected. Please try again! ")
     }

     if ((length(input$country7) > 1) & ("all" %in% input$country7)){
       stop("If 'all' is selected in a given dropdown, then other options in that dropdown should not be selected. Please try again! ")
     }

     if ((length(input$state7) > 1) & ("all" %in% input$state7)){
       stop("If 'all' is selected in a given dropdown, then other options in that dropdown should not be selected. Please try again! ")
     }

     serve_summary_data7 <- gadm_level_summary(gadm2_aqli_2021, c("continent", "country", "name_1"), input$years7)
     if((sum(input$continent7 == "all") == 1) & (sum(input$country7 == "all") == 1) & (sum(input$state7 == "all") == 1)){
       serve_summary_data7
     } else if ((sum(input$continent7 != "all") == length(input$continent7)) & (sum(input$country7 == "all") == 1) & (sum(input$state7 == "all") == 1)){
       serve_summary_data7 %>%
         dplyr::filter(continent %in% input$continent7)
     } else if ((sum(input$continent7 != "all") == length(input$continent7)) & (sum(input$country7 != "all") == length(input$country7)) & (sum(input$state7 == "all") == 1)){
       serve_summary_data7 %>%
         dplyr::filter(continent %in% input$continent7) %>%
         dplyr::filter(country %in% input$country7)
     } else if ((sum(input$continent7 != "all") == length(input$continent7)) & (sum(input$country7 != "all") == length(input$country7)) & (sum(input$state7 != "all") == length(input$state7))){
       serve_summary_data7 %>%
         dplyr::filter(continent %in% input$continent7) %>%
         dplyr::filter(country %in% input$country7) %>%
         dplyr::filter(name_1 %in% input$state7)
     }


   } else if (input$comparison_level7 == "District"){

     if((length(input$continent7) == 0) | (length(input$country7) == 0) | (length(input$state7) == 0) | (length(input$district7) == 0)){
       stop("Continent, Country, State and District values are needed for a District level comparison! Please select atleast one continent-country-state-district combination to proceed.")
     }

     if ((length(input$continent7) > 1) & ("all" %in% input$continent7)){
       stop("If 'all' is selected in a given dropdown, then other options in that dropdown should not be selected. Please try again! ")
     }

     if ((length(input$country7) > 1) & ("all" %in% input$country7)){
       stop("If 'all' is selected in a given dropdown, then other options in that dropdown should not be selected. Please try again! ")
     }

     if ((length(input$state7) > 1) & ("all" %in% input$state7)){
       stop("If 'all' is selected in a given dropdown, then other options in that dropdown should not be selected. Please try again! ")
     }

     if ((length(input$district7) > 1) & ("all" %in% input$district7)){
       stop("If 'all' is selected in a given dropdown, then other options in that dropdown should not be selected. Please try again! ")
     }

     serve_summary_data7 <- gadm_level_summary(gadm2_aqli_2021, c("continent", "country", "name_1", "name_2"), input$years7)
     if((sum(input$continent7 == "all") == 1) & (sum(input$country7 == "all") == 1) & (sum(input$state7 == "all") == 1) & (sum(input$district7 == "all") == 1)){
       serve_summary_data7
     } else if ((sum(input$continent7 != "all") == length(input$continent7)) & (sum(input$country7 == "all") == 1) & (sum(input$state7 == "all") == 1) & (sum(input$district7 == "all") == 1)){
       serve_summary_data7 %>%
         dplyr::filter(continent %in% input$continent7)
     } else if ((sum(input$continent7 != "all") == length(input$continent7)) & (sum(input$country7 != "all") == length(input$country7)) & (sum(input$state7 == "all") == 1) & (sum(input$district7 == "all") == 1)){
       serve_summary_data7 %>%
         dplyr::filter(continent %in% input$continent7) %>%
         dplyr::filter(country %in% input$country7)
     } else if ((sum(input$continent7 != "all") == length(input$continent7)) & (sum(input$country7 != "all") == length(input$country7)) & (sum(input$state7 != "all") == length(input$state7)) & (sum(input$district7 == "all") == 1)){
       serve_summary_data7 %>%
         dplyr::filter(continent %in% input$continent7) %>%
         dplyr::filter(country %in% input$country7) %>%
         dplyr::filter(name_1 %in% input$state7)
     } else if ((sum(input$continent7 != "all") == length(input$continent7)) & (sum(input$country7 != "all") == length(input$country7)) & (sum(input$state7 != "all") == length(input$state7)) & (sum(input$district7 != "all") == length(input$district7))){
       serve_summary_data7 %>%
         dplyr::filter(continent %in% input$continent7) %>%
         dplyr::filter(country %in% input$country7) %>%
         dplyr::filter(name_1 %in% input$state7) %>%
         dplyr::filter(name_2 %in% input$district7)
     }



   }

 })


 # render the filtered dataset
 output$table7 <- DT::renderDataTable(
   if(input$compare_regions_button7 == 0){
     return(NULL)
   } else {
     options = list(extensions = "Buttons")
     filteredData7()
   }
 )

 # download button of the filtered dataset
 output$downloadData7 <- downloadHandler(
   filename = "region_comparison_data.csv",
   content = function(file) {
     write.csv(filteredData7(), file)
   }
 )

# plot 1 from the filtered data
output$plot_7_1 <-

  shiny::renderPlot({
    if(input$comparison_level7 == "Continent"){

      filteredData7() %>%
        tidyr::pivot_longer(dplyr::starts_with("pm"), names_to = "year", values_to = "pm2.5_pollution") %>%
        dplyr::mutate(year = as.numeric(str_remove(year, "pm"))) %>%
        dplyr::select(objectid_level:whostandard, year, pm2.5_pollution) %>%
        dplyr::mutate(year = as.factor(year)) %>%
        ggplot2::ggplot() +
        ggplot2::geom_col(mapping = aes(x = forcats::fct_reorder(continent, pm2.5_pollution), y = pm2.5_pollution, fill = year)) +
        ggplot2::coord_flip() +
        ggplot2::facet_wrap(.~year) +
        ggplot2::labs(x = "Continent", y = expression("Annual Average" ~ PM[2.5] ~ "(in µg/m³)"), title = expression(PM[2.5] ~ "Pollution"),
             fill = "Year") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        ggthemes::theme_clean() +
        ggplot2::theme(plot.title = element_text(hjust = 0.5),
              axis.line = element_line(),
              legend.title = element_text(hjust = 0.5),
              legend.position = "bottom") %>%
        return()



    } else if (input$comparison_level7 == "Country"){

      filteredData7() %>%
        tidyr::pivot_longer(dplyr::starts_with("pm"), names_to = "year", values_to = "pm2.5_pollution") %>%
        dplyr::mutate(year = as.numeric(str_remove(year, "pm"))) %>%
        dplyr::select(objectid_level:natstandard, year, pm2.5_pollution) %>%
        dplyr::mutate(year = as.factor(year)) %>%
        ggplot2::ggplot() +
        ggplot2::geom_col(mapping = aes(x = forcats::fct_reorder(country, pm2.5_pollution), y = pm2.5_pollution, fill = year)) +
        ggplot2::coord_flip() +
        ggplot2::facet_wrap(.~year) +
        ggplot2::labs(x = "Country", y = expression("Annual Average" ~ PM[2.5] ~ "(in µg/m³)"), title = expression(PM[2.5] ~ "Pollution"),
             fill = "Year") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        ggthemes::theme_clean() +
        ggplot2::theme(plot.title = element_text(hjust = 0.5),
              axis.line = element_line(),
              legend.title = element_text(hjust = 0.5),
              legend.position = "bottom") %>%
        return()

    } else if (input$comparison_level7 == "State"){

      filteredData7() %>%
        tidyr::pivot_longer(dplyr::starts_with("pm"), names_to = "year", values_to = "pm2.5_pollution") %>%
        dplyr::mutate(year = as.numeric(str_remove(year, "pm"))) %>%
        dplyr::select(objectid_level:natstandard, year, pm2.5_pollution) %>%
        dplyr::mutate(year = as.factor(year)) %>%
        ggplot2::ggplot() +
        ggplot2::geom_col(mapping = aes(x = forcats::fct_reorder(name_1, pm2.5_pollution), y = pm2.5_pollution, fill = year)) +
        ggplot2::coord_flip() +
        ggplot2::facet_wrap(.~year) +
        ggplot2::labs(x = "State", y = expression("Annual Average" ~ PM[2.5] ~ "(in µg/m³)"), title = expression(PM[2.5] ~ "Pollution"),
             fill = "Year") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        ggthemes::theme_clean() +
        ggplot2::theme(plot.title = element_text(hjust = 0.5),
              axis.line = element_line(),
              legend.title = element_text(hjust = 0.5),
              legend.position = "bottom") %>%
        return()

    } else if (input$comparison_level7 == "District"){
      filteredData7() %>%
        tidyr::pivot_longer(dplyr::starts_with("pm"), names_to = "year", values_to = "pm2.5_pollution") %>%
        dplyr::mutate(year = as.numeric(str_remove(year, "pm"))) %>%
        dplyr::select(objectid_gadm2:natstandard, year, pm2.5_pollution) %>%
        dplyr::mutate(year = as.factor(year)) %>%
        ggplot2::ggplot() +
        ggplot2::geom_col(mapping = aes(x = forcats::fct_reorder(name_2, pm2.5_pollution), y = pm2.5_pollution, fill = year)) +
        ggplot2::coord_flip() +
        ggplot2::facet_wrap(.~year) +
        ggplot2::labs(x = "District/County/Prefecture", y = expression("Annual Average" ~ PM[2.5] ~ "(in µg/m³)"), title = expression(PM[2.5] ~ "Pollution"),
             fill = "Year") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        ggthemes::theme_clean() +
        ggplot2::theme(plot.title = element_text(hjust = 0.5),
              axis.line = element_line(),
              legend.title = element_text(hjust = 0.5),
              legend.position = "bottom") %>%
        return()
    }
  })


# plot 2 from the filtered data

output$plot_7_2 <-

  shiny::renderPlot({
  if(input$comparison_level7 == "Continent"){

    filteredData7() %>%
      tidyr::pivot_longer(dplyr::starts_with("llpp_who"), names_to = "year", values_to = "lyl_who") %>%
      dplyr::mutate(year = as.numeric(str_remove(year, "llpp_who_"))) %>%
      dplyr::select(objectid_level:whostandard, year, lyl_who) %>%
      dplyr::mutate(year = as.factor(year)) %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(mapping = aes(x = forcats::fct_reorder(continent, lyl_who), y = lyl_who, fill = year)) +
      ggplot2::coord_flip() +
      ggplot2::facet_wrap(.~year) +
      ggplot2::labs(x = "Continent", y = "Life Years Lost (rel to WHO guideline)", title = "Life Years Lost",
           fill = "Year") +
      viridis::scale_fill_viridis(discrete = TRUE) +
      ggthemes::theme_clean() +
      ggplot2::theme(plot.title = element_text(hjust = 0.5),
            axis.line = element_line(),
            legend.title = element_text(hjust = 0.5),
            legend.position = "bottom") %>%
      return()



  } else if (input$comparison_level7 == "Country"){

    filteredData7() %>%
      tidyr::pivot_longer(dplyr::starts_with("llpp_who"), names_to = "year", values_to = "lyl_who") %>%
      dplyr::mutate(year = as.numeric(str_remove(year, "llpp_who_"))) %>%
      dplyr::select(objectid_level:natstandard, year, lyl_who) %>%
      dplyr::mutate(year = as.factor(year)) %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(mapping = aes(x = forcats::fct_reorder(country, lyl_who), y = lyl_who, fill = year)) +
      ggplot2::coord_flip() +
      ggplot2::facet_wrap(.~year) +
      ggplot2::labs(x = "Country", y = "Life Years Lost (rel to WHO guideline)", title = "Life Years Lost",
           fill = "Year") +
      viridis::scale_fill_viridis(discrete = TRUE) +
      ggthemes::theme_clean() +
      ggplot2::theme(plot.title = element_text(hjust = 0.5),
            axis.line = element_line(),
            legend.title = element_text(hjust = 0.5),
            legend.position = "bottom") %>%
      return()


  } else if (input$comparison_level7 == "State"){

    filteredData7() %>%
      tidyr::pivot_longer(dplyr::starts_with("llpp_who"), names_to = "year", values_to = "lyl_who") %>%
      dplyr::mutate(year = as.numeric(str_remove(year, "llpp_who_"))) %>%
      dplyr::select(objectid_level:natstandard, year, lyl_who) %>%
      dplyr::mutate(year = as.factor(year)) %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(mapping = aes(x = forcats::fct_reorder(name_1, lyl_who), y = lyl_who, fill = year)) +
      ggplot2::coord_flip() +
      ggplot2::facet_wrap(.~year) +
      ggplot2::labs(x = "State", y = "Life Years Lost (rel to WHO guideline)", title = "Life Years Lost",
           fill = "Year") +
      viridis::scale_fill_viridis(discrete = TRUE) +
      ggthemes::theme_clean() +
      ggplot2::theme(plot.title = element_text(hjust = 0.5),
            axis.line = element_line(),
            legend.title = element_text(hjust = 0.5),
            legend.position = "bottom") %>%
      return()

  } else if (input$comparison_level7 == "District"){

    filteredData7() %>%
      tidyr::pivot_longer(dplyr::starts_with("llpp_who"), names_to = "year", values_to = "lyl_who") %>%
      dplyr::mutate(year = as.numeric(str_remove(year, "llpp_who_"))) %>%
      dplyr::select(objectid_gadm2:natstandard, year, lyl_who) %>%
      dplyr::mutate(year = as.factor(year)) %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(mapping = aes(x = forcats::fct_reorder(name_2, lyl_who), y = lyl_who, fill = year)) +
      ggplot2::coord_flip() +
      ggplot2::facet_wrap(.~year) +
      ggplot2::labs(x = "District/County/Prefecture", y = "Life Years Lost (rel to WHO guideline)", title = "Life Years Lost",
           fill = "Year") +
      viridis::scale_fill_viridis(discrete = TRUE) +
      ggthemes::theme_clean() +
      ggplot2::theme(plot.title = element_text(hjust = 0.5),
            axis.line = element_line(),
            legend.title = element_text(hjust = 0.5),
            legend.position = "bottom") %>%
      return()

  }
})








#---------------------------------------------------------------------------------------



}


shiny::shinyApp(ui, server)
