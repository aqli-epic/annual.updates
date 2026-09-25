# Define colors for each cause
cause_colors <- c(
  "Child and maternal malnutrition" = "#6BAED6",
  "Dietary risks" = "#A1B56C",
  "PM2.5 relative to WHO guideline" = "#A94442",
  "Tobacco" = "#6A51A3",
  "Unsafe water, sanitation, and handwashing" = "#F4A261"
)


top_causes <- c(
  "Child and maternal malnutrition",
  "Dietary risks",
  "PM2.5 relative to WHO guideline",
  "Tobacco",
  "Unsafe water, sanitation, and handwashing"
)




output$gbd_top_10_risk <- renderHighchart({
  req(input$country_gis_gbd)
  
  filtered_data <- gbd_results_master_2025 %>%
    filter(
      country %in% input$country_gis_gbd,
      cause_of_death %in% top_causes
    )
  # Filter top causes and selected countries
 
  
  # Pivot to wide format for highcharter grouped columns
  chart_data <- filtered_data %>%
    mutate(cause_of_death = factor(cause_of_death, levels = top_causes)) %>%
    tidyr::pivot_wider(names_from = cause_of_death, values_from = lyl, values_fill = 0)
  

  
  # Initialize chart
  hc <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = chart_data$country) %>%
    hc_yAxis(title = list(text = "Life Years Lost"), max = 5.5) %>%
    hc_plotOptions(column = list(grouping = TRUE)) %>%
    hc_title(text = "Comparison of selected major global threats to life expectancy") %>%
    hc_legend(title = list(text = "Threats to Life Expectancy")) %>%
    hc_tooltip(shared = TRUE, valueDecimals = 2, valueSuffix = " years") %>%
    hc_exporting(enabled = TRUE)
  
  # Add each series explicitly
  for (cause in top_causes) {
    hc <- hc %>%
      hc_add_series(
        name = cause,
        data = chart_data[[cause]],
        color = cause_colors[[cause]]
      )
  }
  
  hc
  # return the chart object
})


output$top_10_gbd_cause <- renderHighchart({
  # Get top 10
data <- gbd_results_master_2023 %>% filter(country %in% input$country_gis_gbd) %>% 
    arrange(desc(lyl)) %>% mutate(lyl = round(lyl,1)) %>% head(input$gbdslider)

print("=======================================00000")
print(input$gbdslider)
value_bins <- cut(
  data$lyl,
  breaks = c(-Inf, 0.1, 0.5, 1, 2, 3, 4, 5, 6, Inf),
  labels = c("0 - < 0.1 years", "0.1 - 0.5", "> 0.5 - 1", "> 1 - 2",
             "> 2 - 3", "> 3 - 4", "> 4 - 5", "> 5 - < 6", ">= 6"),
  right = FALSE
)

# Define your custom color palette
color_map <- c(
  "0 - < 0.1 years" = "#FFFFFF",
  "0.1 - 0.5" = "#FFE6B3",
  "> 0.5 - 1" = "#FFD25D",
  "> 1 - 2" = "#FFBA00",
  "> 2 - 3" = "#FF9600",
  "> 3 - 4" = "#FF6908",
  "> 4 - 5" = "#E63D23",
  "> 5 - < 6" = "#BD251C",
  ">= 6" = "#8C130E"
)

# Add bin and color to data
data$bin <- value_bins
data$color <- color_map[as.character(data$bin)]


highchart() %>%
  hc_chart(type = "bar") %>%
  hc_title(text = "Top 10 threats to life expectancy in China") %>%
  hc_title(
    text = paste0(
      "Top ", input$gbdslider, " threats to life expectancy in <span style='color:maroon;'>", unique(input$country_gis_gbd), 
      "</span>"
    ),
    useHTML = TRUE
  ) %>% 
  hc_xAxis(categories = data$cause_of_death,
           title = list(text = "Cause of Death")) %>%
  hc_yAxis(title = list(text = "Years of Life Lost")) %>%
  hc_add_series(
    type = "bar",
    name = "Years Lost",
    data = purrr::map2(data$lyl, data$color, ~ list(y = .x, color = .y))
  ) %>%
  hc_tooltip(pointFormat = "<b>{point.y}</b> years lost") %>%
  hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE))) %>%
  hc_legend(enabled = FALSE)


})


# output$top_10_polluted_populated_who <- renderHighchart({
# 
#   # Sample data (replace with your actual data)
# 
#   
#   # Get top 10
#   top10 <- gbd_results_master_2023 %>% filter(country %in% "India") %>%
#     arrange(desc(lyl)) %>% mutate(lyl = round(lyl,1)) %>% 
#     slice(1:10)
#   
#   # Define gradient from dark orange to light orange
#   colors <- colorRampPalette(c("#8C130E", "#FFD580"))(10)  # dark orange to light
#   
#   # Create highchart
#   highchart() %>%
#     hc_chart(type = "bar") %>%
#     hc_title(text = "Top 10 Causes of Death") %>%
#     hc_xAxis(categories = top10$cause_of_death,
#              title = list(text = "Cause of Death"),
#              labels = list(style = list(fontSize = "13px"))) %>%
#     hc_yAxis(title = list(text = "Number of Deaths")) %>%
#     hc_add_series(name = "Deaths",
#                   data = top10$lyl,
#                   colorByPoint = TRUE,
#                   colors = colors) %>%
#     hc_tooltip(pointFormat = "<b>{point.y:,.0f}</b> Life Years Loss") %>%
#     hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE))) %>%
#     hc_legend(enabled = FALSE)
#   
#   
# })

