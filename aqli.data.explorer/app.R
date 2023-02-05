library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(shinythemes)
library(mapdata)
library(DT)

# Define UI
ui <- shiny::fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("AQLI Data Explorer"),
  tabsetPanel(
    tabPanel("Raw Data Explore",
             dataTableOutput("data_table")),
    tabPanel("Regional Data Exploration",
             fluidRow(
               column(3, selectInput("continent", "Continent:", c("Africa", "Asia", "Europe", "North America", "South America", "Oceania"))),
               column(3, selectInput("country", "Country:", "")),
               column(3, selectInput("state", "State:", "")),
               column(3, selectInput("district", "District:", "")),
             ),
             fluidRow(
               column(12, dataTableOutput("filtered_table"))
             ),
             downloadButton("downloadData", "Download")
    ),
    tabPanel("Other Summary Stats",
             fluidRow(
               column(6, numericInput("num", "Top n:", 5, min = 1, max = 100)),
               column(6, selectInput("stat", "Most:", c("Polluted", "Populated"))),
             ),
             fluidRow(
               column(6, tableOutput("summary_table")),
               column(6, plotOutput("summary_plot"))
             )
    ),
    tabPanel("Map Exploration",
             fluidRow(
               column(3, selectInput("map_country", "Country:", "")),
               column(3, selectInput("map_state", "State:", "")),
               column(3, selectInput("map_district", "District:", "")),
             ),
             fluidRow(
               column(12, leafletOutput("map"))
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Load data
  data("mtcars")
  # data("world")

  # Raw data explore tab
  output$data_table <- renderDataTable({
    mtcars
  })

  # Regional data exploration tab
  # observeEvent(input$continent, {
  #   updateSelectInput(session, "country", "Country:", subset(world, continent == input$continent)$name)
  # })
  # observeEvent(input$country, {
  #   updateSelectInput(session, "state", "State:", subset(world, name == input$country)$subregion)
  # })
  # observeEvent(input$state, {
  #   updateSelectInput(session, "district", "District:", subset(world, subregion == input$state)$region)
  # })
  #
  # filtered_data <- reactive({
  #   if (is.null(input$district)) return(NULL)
  #   subset(world, region == input$district)
  # })

  output$filtered_table <- renderDataTable({
    filtered_data()
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )

  # other summary stats tab
  summary_data <- reactive({
    if (input$stat == "Polluted") {
      return(mtcars %>%
               arrange(desc(mpg)) %>%
               head(input$num))
    } else {
      return(mtcars %>%
               arrange(desc(wt)) %>%
               head(input$num))
    }
  })

  output$summary_table <- renderTable({
    summary_data()
  })

  output$summary_plot <- renderPlot({
    if (input$stat == "Polluted") {
      ggplot(summary_data(), aes(x = rownames(summary_data()), y = mpg)) +
        geom_bar(stat = "identity") +
        xlab("Cars") + ylab("MPG")
    } else {
      ggplot(summary_data(), aes(x = rownames(summary_data()), y = wt)) +
        geom_bar(stat = "identity") +
        xlab("Cars") + ylab("Weight")
    }
  })

  # # map exploration tab
  # observeEvent(input$map_country, {
  #   updateSelectInput(session, "map_state", "State:", subset(world, name == input$map_country)$subregion)
  # })
  # observeEvent(input$map_state, {
  #   updateSelectInput(session, "map_district", "District:", subset(world, subregion == input$map_state)$region)
  # })
  #
  # map_data <- reactive({
  #   if (is.null(input$map_district)) return(NULL)
  #   subset(world, region == input$map_district)
  # })
  #
  # output$map <- renderLeaflet({
  #   leaflet(map_data()) %>%
  #     addTiles() %>%
  #     addPolygons()
  # })
}

shiny::shinyApp(ui = ui, server = server)


