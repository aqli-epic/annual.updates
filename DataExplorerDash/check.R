library(shiny)
library(highcharter)
library(dplyr)

gadm0_aqli_2023_summary <- gadm0_aqli_2023 %>% 

  library(dplyr)
library(tidyr)

# Filter out zero population rows
filtered_data <- gadm0_aqli_2023 %>%
  filter(population > 0)

# Pivot PM2.5 columns
pm_long <- filtered_data %>% select(c("country", "population", "whostandard", "natstandard"),starts_with("pm")) %>% 
  pivot_longer(!c("country", "population", "whostandard", "natstandard"),
    names_to = "year",
  #  names_prefix = "pm",
    values_to = "pm25"
  ) %>% mutate(year = as.integer(str_remove(year, "pm")))


# Calculate population-weighted PM2.5 per year
pop_weighted_pm25 <- pm_long %>%
  group_by(year) %>%
  summarise(
    total_population = sum(population, na.rm = TRUE),
    pop_weighted_pm25 = round(sum(pm25 * population, na.rm = TRUE) / total_population, 2),
    .groups = "drop"
  )


# Pivot LLPP (WHO)
llpp_who_long <-filtered_data %>% select(c("country", "population", "whostandard", "natstandard"),starts_with("llpp_who_")) %>% 
  pivot_longer(!c("country", "population", "whostandard", "natstandard"),
    names_to = "year",
    values_to = "llpp_who"
  ) %>%
  mutate(year = as.integer(str_remove(year, "llpp_who_")))

pop_weighted_who_llpp <- llpp_who_long %>%
  group_by(year) %>%
  summarise(
    total_population = sum(population, na.rm = TRUE),
    pop_weighted_who_llpp = round(sum(llpp_who * population, na.rm = TRUE) / total_population, 2),
    .groups = "drop"
  )

# Pivot LLPP (National)
llpp_nat_long <- filtered_data %>% select(c("country", "population", "whostandard", "natstandard"),starts_with("llpp_nat_")) %>% 
  pivot_longer(!c("country", "population", "whostandard", "natstandard"),
    names_to = "year",
    values_to = "llpp_nat"
  ) %>%
  mutate(year = as.integer(str_remove(year, "llpp_nat_")))

pop_weighted_nat_llpp <- llpp_nat_long %>%
  group_by(year) %>%
  summarise(
    total_population = sum(population, na.rm = TRUE),
    pop_weighted_nat_llpp = round(sum(llpp_nat * population, na.rm = TRUE) / total_population, 2),
    .groups = "drop"
  ) 


combined_long <- pop_weighted_pm25 %>%
  left_join(pop_weighted_who_llpp %>% select(-c(total_population)), by = c("year")) %>%
  left_join(pop_weighted_nat_llpp %>% select(-c(total_population)), by = c("year"))

head(pop_weighted_summary)


# Simulated trend data
spark_data <- tibble(
  year = 2015:2024,
  pm25 = c(92, 90, 86, 83, 80, 75, 70, 67, 63, 60),
  pop_who = c(95, 94, 94, 93, 92, 91, 90, 88, 86, 85),
  pop_nat = c(80, 79, 78, 77, 76, 75, 74, 73, 72, 71),
  le_loss_who = rep(4.5, 10),
  le_loss_nat = seq(1.2, 1.8, length.out = 10)
)

ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      body {
        background-color: #f8f9fa;
        font-family: 'Inter', sans-serif;
      }
      .aqli-box {
        background-color: #2c3e50;
        color: #ffffff;
        border-radius: 16px;
        padding: 20px 24px;
        margin-bottom: 24px;
        box-shadow: 0 4px 10px rgba(0, 0, 0, 0.2);
        height: 100%;
      }
      .aqli-title {
        font-size: 15px;
        font-weight: 600;
        color: #ffb347;
        margin-bottom: 6px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .aqli-value {
        font-size: 30px;
        font-weight: 700;
        margin-bottom: 6px;
      }
      .aqli-sub {
        font-size: 13px;
        color: #d0d3d4;
        margin-bottom: 10px;
      }
      .aqli-trend {
        font-size: 12px;
        font-style: italic;
        color: #fdd9a0;
      }
    "))
  ),
  
  #div(class = "aqli-trend", paste0("↓ from ", round((combined_long$pop_weighted_pm25[combined_long$year == max(combined_long$year)] - combined_long$pop_weighted_pm25[combined_long$year == max(combined_long$year-10)])/ combined_long$pop_weighted_pm25[combined_long$year == max(combined_long$year-10)]*100), " µg/m³ in 2015")),
  
  
  fluidRow(
    column(3,
           div(class = "aqli-box",
               div(class = "aqli-title", "National Avg PM2.5"),
               div(class = "aqli-value", combined_long$pop_weighted_pm25[combined_long$year == max(combined_long$year)]),
               div(class = "aqli-sub", "Latest annual average (2023)"),
               div(class = "aqli-trend", paste0("↓ from ", combined_long$pop_weighted_pm25[combined_long$year == max(combined_long$year-10)], " µg/m³ in ", max(combined_long$year-10, na.rm = T))),
               highchartOutput("spark_pm25", height = "70px")
           )
    ),
    column(3,
           div(class = "aqli-box",
               div(class = "aqli-title", "% Population > WHO Std."),
               div(class = "aqli-value", round(gadm2_aqli_2023 %>% select(population,pm2023) %>% filter(pm2023>5) %>% summarise(count = sum(population, na.rm = T),1) %>% pull(count)*100/sum(gadm2_aqli_2023$population, na.rm = T))),
               div(class = "aqli-sub", "Exposed to PM2.5 > 5 µg/m³"),
               div(class = "aqli-trend", paste0("↓ from ", gadm2_aqli_2023 %>% select(population,pm2013) %>% filter(pm2013>5) %>% summarise(count = sum(population, na.rm = T)) %>% pull(count)*100/sum(gadm2_aqli_2023$population, na.rm = T), " in ", max(combined_long$year-10, na.rm = T))),
               highchartOutput("spark_who", height = "70px")
           ),

    ),
    column(3,
           div(class = "aqli-box",
               div(class = "aqli-title", "% Population > National Standard"),
               div(class = "aqli-value", "71%"),
               div(class = "aqli-sub", "Exposed to PM2.5 > 40 µg/m³"),
               div(class = "aqli-trend", "↓ from 80% in 2015"),
               highchartOutput("spark_nat", height = "70px")
           )
    ),
    column(3,
           div(class = "aqli-box",
               div(class = "aqli-title", "Avg LE Loss (WHO / Nat)"),
               div(class = "aqli-value", "4.5 / 1.8 yrs"),
               div(class = "aqli-sub", "Due to PM2.5 exposure"),
               div(class = "aqli-trend", "National loss ↑ from 1.2 yrs in 2015"),
               highchartOutput("spark_le", height = "70px")
           )
    )
  )
)

server <- function(input, output, session) {
  
  output$spark_pm25 <- renderHighchart({
    hchart(combined_long, "line", hcaes(x = year, y = pop_weighted_pm25)) %>%
      hc_chart(backgroundColor = "transparent", margin = c(2, 0, 2, 0)) %>%
      hc_xAxis(visible = FALSE) %>%
      hc_yAxis(visible = FALSE) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_legend(enabled = FALSE) %>%
      hc_plotOptions(line = list(color = "#ffb347", marker = list(enabled = FALSE)))
  })
  
  output$spark_who <- renderHighchart({
    hchart(combined_long, "line", hcaes(x = year, y = pop_weighted_who_llpp)) %>%
      hc_chart(backgroundColor = "transparent", margin = c(2, 0, 2, 0)) %>%
      hc_xAxis(visible = FALSE) %>%
      hc_yAxis(visible = FALSE) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_legend(enabled = FALSE) %>%
      hc_plotOptions(line = list(color = "#ffa94d", marker = list(enabled = FALSE)))
  })
  
  output$spark_nat <- renderHighchart({
    hchart(combined_long, "line", hcaes(x = year, y = pop_weighted_nat_llpp)) %>%
      hc_chart(backgroundColor = "transparent", margin = c(2, 0, 2, 0)) %>%
      hc_xAxis(visible = FALSE) %>%
      hc_yAxis(visible = FALSE) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_legend(enabled = FALSE) %>%
      hc_plotOptions(line = list(color = "#ff7849", marker = list(enabled = FALSE)))
  })
  
  output$spark_le <- renderHighchart({
    hchart(combined_long, "line", hcaes(x = year, y = total_population)) %>%
      hc_chart(backgroundColor = "transparent", margin = c(2, 0, 2, 0)) %>%
      hc_xAxis(visible = FALSE) %>%
      hc_yAxis(visible = FALSE) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_legend(enabled = FALSE) %>%
      hc_plotOptions(line = list(color = "#f4a261", marker = list(enabled = FALSE)))
  })
}

shinyApp(ui, server)
