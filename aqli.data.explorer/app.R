

# download libraries
library(shiny)
library(shinyWidgets)
library(tidyverse)

# source data
source("C:/Users/Aarsh/Desktop/aqli-epic/annual.updates/R/aqli.data.explorer.helper.script.R")

# Define UI
ui <- fluidPage(
  titlePanel("AQLI data explorer"),
  tabsetPanel(
    tabPanel("Raw Data Exploration",
             fluidRow(
               column(3, selectizeInput("continent", "Continent", choices = c("World", unique(gadm2_aqli_2021$continent)))),
               column(3, selectizeInput("country", "Country", choices = NULL)),
               column(3, selectizeInput("state", "State", choices = NULL)),
               column(3, selectizeInput("district", "District", choices = NULL))
             ),
             dataTableOutput("table"),
             downloadButton("downloadData", "Download")
    ),
    tabPanel("Regional Stats",
             fluidRow(
               column(2, sliderInput("top", "Top", min = 1, max = 50, value = 10)),
               column(2, selectInput("most", "Most", choices = c("Populated", "Polluted"), selected = "Populated")),
               column(2, selectInput("region_type", "Region Type", choices = c("State(s)", "District(s)"), selected = "State(s)")),
               column(1, textOutput("inText")),
               column(2, selectizeInput("country2", "Country", choices = unique(gadm2_aqli_2021$country))),
               column(1, textOutput("inText2")),
               column(2, selectInput("year", "Year", choices = first_year:latest_year, selected = latest_year))
             ),
             fluidRow(
               column(6, dataTableOutput("table2")),
               column(6, plotOutput("plot"))
             ),
             downloadButton("downloadData2", "Download")
    ),
    tabPanel("Trendlines",
             h2("Coming soon...")
    ),
    tabPanel("Pollution/LYL Maps",
             fluidRow(
               column(3, selectizeInput("country3", "Country", choices = NULL)),
               column(3, selectInput("level", "Level", choices = c("Pollution", "LYL"), selected = "Pollution")),
               column(3, actionButton("generateMap", "Generate Map"))
             ),
             plotOutput("map")
    )
  )
)

# Define server
server <- function(input, output, session) {

# Raw data exploration tab---------------------------------------------------
observeEvent(input$continent, {
  updateSelectizeInput(session, "country", choices = c("all", gadm2_aqli_2021 %>% filter(continent == input$continent) %>% pull(country) %>% unique() ))

})


observeEvent(input$country, {
    updateSelectizeInput(session, "state", choices = c("all", gadm2_aqli_2021 %>% filter(country == input$country) %>% pull(name_1) %>% unique()))

  })


observeEvent(input$state, {
  updateSelectizeInput(session, "district", choices = c("all", gadm2_aqli_2021 %>% filter(name_1 == input$state) %>% pull(name_2) %>% unique() ))

})


#
filteredData <- reactive({
    if(input$continent == "World"){
      gadm2_aqli_2021
    } else {
      if(input$country == "all"){
        gadm2_aqli_2021 %>%
          filter(continent == input$continent)
      } else{
        if(input$state == "all"){
          gadm2_aqli_2021 %>%
            filter(continent == input$continent, country == input$country)
        } else{
          if(input$district == "all"){
            gadm2_aqli_2021 %>%
              filter(continent == input$continent, country == input$country, name_1 == input$state)
          } else {
            gadm2_aqli_2021 %>%
              filter(continent == input$continent, country == input$country, name_1 == input$state, name_2 == input$district)
          }

        }
      }
    }

  })

  output$table <- renderDataTable({
    filteredData()
  })


  output$downloadData <- downloadHandler(
    filename = "filtered_data.csv",
    content = function(file) {
      write.csv(filteredData(), file)
    }
  )


#> Regional Stats tab----------------------------------------------------

  filteredData2 <- reactive({
    pol_col <- str_c("pm", input$year)
    llpp_who_col <- str_c("llpp_who_", input$year)
    llpp_nat_col <- str_c("llpp_nat_", input$year)
    if(input$region_type == "State(s)"){
      if(input$most == "Populated"){
        gadm1_aqli_2021 %>%
          filter(country == input$country2) %>%
            select(country, name_1, population, natstandard, !!as.symbol(pol_col), !!as.symbol(llpp_who_col), !!as.symbol(llpp_nat_col)) %>%
            slice_max(population, n = input$top)

      } else if (input$most == "Polluted"){
        gadm1_aqli_2021 %>%
          filter(country == input$country2) %>%
          select(country, name_1, population, natstandard, !!as.symbol(pol_col), !!as.symbol(llpp_who_col), !!as.symbol(llpp_nat_col)) %>%
          slice_max(!!as.symbol(pol_col), n = input$top)
      }

    } else if (input$region_type == "District(s)") {
        if(input$most == "Populated"){
          gadm2_aqli_2021 %>%
            filter(country == input$country2) %>%
            select(country, name_1, name_2, population, natstandard, !!as.symbol(pol_col), !!as.symbol(llpp_who_col), !!as.symbol(llpp_nat_col)) %>%
            slice_max(population, n = input$top)

        } else if (input$most == "Polluted"){
          gadm2_aqli_2021 %>%
            filter(country == input$country2) %>%
            select(country, name_1, name_2, population, natstandard !!as.symbol(pol_col), !!as.symbol(llpp_who_col), !!as.symbol(llpp_nat_col)) %>%
            slice_max(!!as.symbol(pol_col), n = input$top)
        }
    }
  })

  output$inText <- renderText("in")
  output$inText2 <- renderText("in")

  output$table2 <- renderDataTable({
    filteredData2()
  })

  output$plot <- renderPlot({
    filteredData2() %>%
      ggplot(aes(x = !!sym(input$most), y = mpg)) +
      geom_point()
  })

  output$downloadData2 <- downloadHandler(
    filename = "filtered_data2.csv",
    content = function(file) {
      write.csv(filteredData2(), file)
    }
  )

  # Pollution/LYL Maps
  observeEvent(input$generateMap, {
    data3 <- mtcars
    filteredData3 <- data3 %>%
      filter(country == input$country3)

    if (input$level == "Pollution") {
      map <- ggplot(filteredData3, aes(x = wt, y = mpg, color = qsec)) +
        geom_point() +
        scale_color_gradient(low = "green", high = "red")
    } else {
      map <- ggplot(filteredData3, aes(x = wt, y = mpg, color = disp)) +
        geom_point() +
        scale_color_gradient(low = "green", high = "red")
    }

    output$map <- renderPlot(map)
  })

}


shinyApp(ui, server)
