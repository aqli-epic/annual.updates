library(shiny)
library(shinyjs)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)

# Sample AQLI-like dataset
aqli_data <- data.frame(
  country = rep(c("India", "China", "Bangladesh"), each = 3),
  district = c("Delhi", "Mumbai", "Kolkata", "Beijing", "Shanghai", "Guangzhou", "Dhaka", "Chittagong", "Khulna"),
  pm25 = runif(9, 40, 100),
  le_loss = runif(9, 1.5, 7.0)
)

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

    /* Hover effect for tab items */
    .desktop-menu a:hover,
    .mobile-menu a:hover {
      color: gold !important;
    }
  "))
  ),
  
  
  # Header bar
  tags$div(
    style = "background-color: #800000; color: white; padding: 8px 20px; display: flex; justify-content: space-between;",
    tags$div(
      style = "display: flex; align-items: center;",
      tags$img(src = "https://res.cloudinary.com/diwsbenwr/image/upload/v1750129379/uchicago_logo_cbiopy.png", height = "25px"),
      tags$span("THE UNIVERSITY OF CHICAGO", style = "margin-left: 10px; font-weight: bold;")
    ),
    tags$div("EPIC · UCHICAGO CLIMATE & GROWTH")
  ),
  
  # Menu bar
  tags$div(
    style = "background-color: white; padding: 10px 30px; display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid #ccc; position: relative;",
    
    tags$div(tags$img(src = "https://res.cloudinary.com/diwsbenwr/image/upload/v1750129467/aqli_logo_g2fu7o.png", height = "60px")),
    
    tags$div(
      class = "desktop-menu",
      style = "display: flex; gap: 25px; font-size: 16px; font-weight: 500;",
      tags$a("The Index", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'index')", style = "color: grey; text-decoration: none;"),
      tags$a("About", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'about')", style = "color: grey; text-decoration: none;"),
      tags$a("Pollution Facts", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'facts')", style = "color: grey; text-decoration: none;"),
      tags$a("Policy Impacts", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'impacts')", style = "color: grey; text-decoration: none;"),
      tags$a("Reports", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'reports')", style = "color: grey; text-decoration: none;"),
      tags$a("News", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'news')", style = "color: grey; text-decoration: none;")
    ),
    
    tags$div(
      style = "display: flex; gap: 10px; align-items: center;",
      #tags$span("ENG | 中文 | हिन्दी", style = "color: grey; font-size: 14px;"),
      tags$img(src = "https://img.icons8.com/ios-filled/20/search--v1.png"),
      tags$img(src = "https://img.icons8.com/ios-filled/20/twitter.png"),
      tags$img(
        class = "mobile-menu-icon",
        src = "https://img.icons8.com/ios-filled/20/menu.png",
        onclick = "
          var menu = document.getElementById('mobileTabs');
          menu.classList.toggle('show');
        "
      )
    ),
    
    tags$div(
      id = "mobileTabs",
      class = "mobile-menu",
      tags$a("The Index", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'index')"),
      tags$a("About", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'about')"),
      tags$a("Pollution Facts", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'facts')"),
      tags$a("Policy Impacts", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'impacts')"),
      tags$a("Reports", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'reports')"),
      tags$a("News", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'news')") 
    )
  ),
  
  # Content sections
  tags$div(
    id = "main-content",
    style = "padding: 30px;",
    
    tags$div(id = "index_section", h2("The Index"), plotlyOutput("index_plot")),
    hidden(tags$div(id = "about_section", h2("About AQLI"), plotlyOutput("about_plot"))),
    hidden(tags$div(id = "facts_section", h2("Pollution Facts"), plotlyOutput("facts_plot"))),
    hidden(tags$div(id = "impacts_section", h2("Policy Impacts"), plotlyOutput("impacts_plot"))),
    hidden(tags$div(id = "reports_section", h2("Reports"), plotlyOutput("reports_plot"))),
    hidden(tags$div(id = "news_section", h2("News"), plotlyOutput("news_plot")))
  )
)

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
  
  output$index_plot <- renderPlotly({
    ggplotly(
      ggplot(aqli_data, aes(x = district, y = le_loss, fill = country)) +
        geom_col() +
        labs(title = "Life Expectancy Loss by District", y = "Years") +
        theme_minimal()
    )
  })
  
  output$about_plot <- renderPlotly({
    ggplotly(
      ggplot(aqli_data, aes(x = pm25, y = le_loss, color = country)) +
        geom_point(size = 3) +
        labs(title = "PM2.5 vs Life Expectancy Loss") +
        theme_minimal()
    )
  })
  
  output$facts_plot <- renderPlotly({
    ggplotly(
      aqli_data %>%
        group_by(country) %>%
        summarise(avg_pm25 = mean(pm25)) %>%
        ggplot(aes(x = country, y = avg_pm25, fill = country)) +
        geom_col() +
        labs(title = "Average PM2.5 by Country", y = "µg/m³") +
        theme_minimal()
    )
  })
  
  output$impacts_plot <- renderPlotly({
    ggplotly(
      ggplot(aqli_data, aes(x = district, y = pm25)) +
        geom_line(aes(group = country, color = country)) +
        labs(title = "PM2.5 Levels Across Districts") +
        theme_minimal()
    )
  })
  
  output$reports_plot <- renderPlotly({
    ggplotly(
      ggplot(aqli_data, aes(x = reorder(district, le_loss), y = le_loss)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(title = "Report: LE Loss by District", y = "Years") +
        theme_minimal()
    )
  })
  
  output$news_plot <- renderPlotly({
    ggplotly(
      ggplot(aqli_data, aes(x = district, y = pm25, size = le_loss, color = country)) +
        geom_point(alpha = 0.7) +
        labs(title = "News Snapshot: PM2.5 and LE Loss") +
        theme_minimal()
    )
  })
}

shinyApp(ui, server)
