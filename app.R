# setting global options
options(shiny.maxRequestSize = 900*1024^2)

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

# source data
source("./R/aqli.data.explorer.helper.script.R")

# loading the .RData file that contain all required data objects
# save(list = ls(all = TRUE), file= "all.RData")
# load("all.RData", .GlobalEnv)

# Define UI
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "litera"),
  title = "AQLI Data Explorer",
  titlePanel(
    fluidRow(
      column(width = 5, img(src = "aqli-epic-data-exp-logo.png", height = "100px"))
      # column(width = 10, h2("AQLI Data Explorer", style = "margin-top: -5px;"), offset = 1)
    )
  ),
  shiny::tags$br(),
  shiny::tags$br(),
  tabsetPanel(
    tabPanel("Raw Data Exploration",
             shiny::tags$br(),
             fluidRow(
               column(3, selectizeInput("continent", "Continent", choices = c("World", unique(gadm2_aqli_2021$continent)))),
               column(3, selectizeInput("country", "Country", choices = NULL)),
               column(3, selectizeInput("state", "State", choices = NULL)),
               column(3, selectizeInput("district", "District", choices = NULL))
             ),
             shiny::tags$br(),
             shiny::tags$hr(),
             fluidRow(
               column(10),
               column(2, downloadButton("downloadData", "Download CSV"))
             ),
             dataTableOutput("table")
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
               column(6, shiny::plotOutput("plot")),
               column(6, shiny::plotOutput("plot_lyl"))
             ),
             shiny::tags$br(),
             shiny::tags$br(),
             shiny::tags$hr(),
             fluidRow(
               column(10),
               column(2, downloadButton("downloadData2", "Download CSV"))
             ),
             fluidRow(
               column(12, dataTableOutput("table2"))
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
             plotOutput("plot_trendlines")
    ),
    tabPanel("Pollution/LYL Maps (coming soon!)",
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
    tabPanel("Compare Regions (coming soon!)"),
    tabPanel("About AQLI (coming soon!)")
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
filteredData <- shiny::reactive({
    if(input$continent == "World"){
      gadm2_aqli_2021
    } else {
      if(input$country == "all"){
        gadm2_aqli_2021 %>%
          dplyr::filter(continent == input$continent)
      } else{
        if(input$state == "all"){
          gadm2_aqli_2021 %>%
            dplyr::filter(continent == input$continent, country == input$country)
        } else{
          if(input$district == "all"){
            gadm2_aqli_2021 %>%
              dplyr::filter(continent == input$continent, country == input$country, name_1 == input$state)
          } else {
            gadm2_aqli_2021 %>%
              dplyr::filter(continent == input$continent, country == input$country, name_1 == input$state, name_2 == input$district)
          }

        }
      }
    }

  })

# render the filtered dataset
  output$table <- shiny::renderDataTable({
    filteredData()
  })

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
  output$table2 <- shiny::renderDataTable({
    filteredData2()
  })

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

    filteredData3 <- gadm2_aqli_2021_shp %>%
      dplyr::filter(country == input$country4)

    if (input$graph_type == "Pollution") {
      map <- filteredData3 %>%
        ggplot2::ggplot() +
        ggplot2::geom_sf(mapping = aes(fill = pm2021)) +
        ggthemes::theme_tufte() +
        viridis::scale_fill_viridis(option = "blues", direction = -1) +
        ggplot2::theme(axis.text = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank()) +
        ggplot2::ggtitle(stringr::str_c("PM2.5 pollution in ", input$country4, " in ", "2021"))

    } else {
      map <- filteredData3 %>%
        ggplot2::ggplot() +
        ggplot2::geom_sf(mapping = aes(fill = llw2021)) +
        ggthemes::theme_tufte() +
        viridis::scale_fill_viridis(option = "rocket", direction = -1) +
        ggplot2::theme(axis.text = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank()) +
        ggplot2::ggtitle(stringr::str_c("Life Years Lost in ", input$country4, " in ", "2021"))


    }

    # output$map <- plotly::renderPlotly(plotly::ggplotly(map))
    output$map <- shiny::renderPlot(map)
  })

}


shiny::shinyApp(ui, server)
