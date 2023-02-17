

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
               shiny::tags$br(),
               column(2, sliderInput("top", "Top", min = 1, max = 50, value = 10)),
               column(2, selectInput("most", "Most", choices = c("Populated", "Polluted"), selected = "Populated")),
               column(2, selectInput("region_type", "Region Type", choices = c("State(s)", "District(s)"), selected = "State(s)")),
               column(1, textOutput("inText")),
               column(2, selectizeInput("country2", "Country", choices = unique(gadm2_aqli_2021$country))),
               column(1, textOutput("inText2")),
               column(2, selectInput("year_range", "Year", choices = first_year:latest_year))
             ),
             fluidRow(
               shiny::tags$hr(),
               column(6, plotOutput("plot")),
               column(6, plotOutput("plot_lyl"))
             ),
             fluidRow(
               shiny::tags$hr(),
               column(12, dataTableOutput("table2")),
               downloadButton("downloadData2", "Download")
             )
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

  # reactive pollution column name
  pol_col <- reactive({
    str_c("pm", input$year_range)
  })

  # reactive llpp_who_col name
  llpp_who_col <- reactive({
    str_c("llpp_nat_", input$year_range)
  })

  # reactive llpp_nat_col name
  llpp_nat_col <- reactive({
    str_c("llpp_nat_", input$year_range)
  })

  # create the filtered dataset for the regional stats tab
  filteredData2 <- reactive({
    llpp_who_col <- str_c("llpp_who_", input$year_range)
    llpp_nat_col <- str_c("llpp_nat_", input$year_range)
    if(input$region_type == "State(s)"){
      if(input$most == "Populated"){
        gadm1_aqli_2021 %>%
          filter(country == input$country2) %>%
            select(country, name_1, population, natstandard, !!as.symbol(pol_col()), !!as.symbol(llpp_who_col()), !!as.symbol(llpp_nat_col())) %>%
            slice_max(population, n = input$top)

      } else if (input$most == "Polluted"){
        gadm1_aqli_2021 %>%
          filter(country == input$country2) %>%
          select(country, name_1, population, natstandard, !!as.symbol(pol_col()), !!as.symbol(llpp_who_col()), !!as.symbol(llpp_nat_col())) %>%
          slice_max(!!as.symbol(pol_col()), n = input$top)
      }

    } else if (input$region_type == "District(s)") {
        if(input$most == "Populated"){
          gadm2_aqli_2021 %>%
            filter(country == input$country2) %>%
            select(country, name_1, name_2, population, natstandard, !!as.symbol(pol_col()), !!as.symbol(llpp_who_col()), !!as.symbol(llpp_nat_col())) %>%
            slice_max(population, n = input$top)

        } else if (input$most == "Polluted"){
          gadm2_aqli_2021 %>%
            filter(country == input$country2) %>%
            select(country, name_1, name_2, population, natstandard, !!as.symbol(pol_col()), !!as.symbol(llpp_who_col()), !!as.symbol(llpp_nat_col())) %>%
            slice_max(!!as.symbol(pol_col()), n = input$top)
        }
    }
  })

  # intermediate text outputs
  output$inText <- renderText("in")
  output$inText2 <- renderText("in")

  # output the filtered data for the regional stats tab
  output$table2 <- renderDataTable({
    filteredData2()
  })

  # plot based on selected values of the dropdown columns in the regional stats tab (pollution graph)
  output$plot <- renderPlot({
    if(input$region_type == "State(s)"){
      if(input$most == "Polluted"){
        filteredData2() %>%
          ggplot(mapping = aes(x = forcats::fct_reorder(name_1, !!as.symbol(pol_col())), y = !!as.symbol(pol_col()))) +
          geom_col(fill = "darkred") +
          coord_flip() +
          ggthemes::theme_hc() +
          labs(x = "State", y = expression(Annual ~ average ~ PM[2.5] ~ concentration ~ "("*"in"*~mu*g*"/"*m^3*")"),
               title = str_c("Pollution Graph: Top ", input$top, " most Polluted States in ", input$country2, " in ", input$year_range))
      } else if (input$most == "Populated"){
        filteredData2() %>%
          ggplot(mapping = aes(x = forcats::fct_reorder(name_1, !!as.symbol(pol_col())), y = !!as.symbol(pol_col()))) +
          geom_col(fill = "darkred") +
          coord_flip() +
          ggthemes::theme_hc() +
          labs(x = "State", y = expression(Annual ~ average ~ PM[2.5] ~ concentration ~ "("*"in"*~mu*g*"/"*m^3*")"),
               title = str_c("Pollution Graph: Top ", input$top, " most Populated States in ", input$country2, " in ", input$year_range))
      }
    } else if (input$region_type == "District(s)"){
      if(input$most == "Polluted"){
        filteredData2() %>%
          ggplot(mapping = aes(x = forcats::fct_reorder(name_2, !!as.symbol(pol_col())), y = !!as.symbol(pol_col()))) +
          geom_col(fill = "darkred") +
          coord_flip() +
          ggthemes::theme_hc() +
          labs(x = "District", y = expression(Annual ~ average ~ PM[2.5] ~ concentration ~ "("*"in"*~mu*g*"/"*m^3*")"),
               title = str_c("Pollution Graph: Top ", input$top, " most Polluted Districts in ", input$country2, " in ", input$year_range))

      } else if (input$most == "Populated"){
        filteredData2() %>%
          ggplot(mapping = aes(x = forcats::fct_reorder(name_2, !!as.symbol(pol_col())), y = !!as.symbol(pol_col()))) +
          geom_col(fill = "darkred") +
          coord_flip() +
          ggthemes::theme_hc() +
          labs(x = "District", y = expression(Annual ~ average ~ PM[2.5] ~ concentration ~ "("*"in"*~mu*g*"/"*m^3*")"),
               title = str_c("Pollution Graph: Top ", input$top, " most Populated Districts in ", input$country2, " in ", input$year_range))
      }
    }
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
