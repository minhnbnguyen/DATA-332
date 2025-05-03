library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(tidyr)
library(DT)

dataset <- readRDS("uber_dataset.rds")

ui <- navbarPage("Uber Trips Dashboard by Minh Nguyen",
                 tabPanel("Line Charts",
                          sidebarLayout(
                            sidebarPanel(selectInput("selected_month","Month:", choices = unique(as.character(dataset$month_name)), selected = as.character(dataset$month_name[1]))
                            ),
                            mainPanel(
                              h4("Number of Trips by Hour"),
                              plotOutput("trips_by_hour", height = "300px"),
                              h4("Daily Number of Trips"),
                              plotOutput("trips_by_day"),
                              h4("Aggregated Number of Trips by Month"),
                              plotOutput("trips_by_month", height = "300px")
                            ))),
                 tabPanel("Bar Charts",
                          fluidPage(
                            h4("Trips by Day and Month"),
                            plotOutput("trips_by_day_month", height = "300px"),
                            h4("Trips by Bases and Month"),
                            plotOutput("trips_by_bases_month", height = "300px")
                          )),
                 tabPanel("Heat Maps",
                          fluidPage(
                            h4("Trips by Hour and Day"),
                            plotOutput("trips_by_hour_day", height = "300px"),
                            h4("Trips by Month and Day"),
                            plotOutput("trips_by_month_day", height = "300px"),
                            h4("Trips by Bases and Weekday"),
                            plotOutput("trips_by_base_weekday", height = "300px")
                          )),
                 tabPanel("Pivot Tables",
                          fluidPage(
                            h4("Trips by Hour"),
                            dataTableOutput("trips_by_hour_pivot", height = "500px"),
                            h4("Trips Every Day"),
                            dataTableOutput("daily_trips", height = "300px")
                          )),
                 tabPanel("Map",
                          leafletOutput("trip_map", height = "500px")),
                 tabPanel("Prediction Model",
                          fluidPage(
                            h4("Linear Regression: Trips by Weekday"),
                            plotOutput("linear_plot", height = "300px")
                          )))

server <- function(input, output, tab) {
  filtered_data <- reactive({
    dataset %>% filter(as.character(month_name) == input$selected_month)
  })
  
  output$trips_by_hour <- renderPlot({
    filtered_data() %>%
      count(hour) %>%
      ggplot(aes(x = hour, y = n, group = 1)) + 
      geom_line(color = "blue", size = 1) +
      labs(
        title = "Number of Uber Trips by Hour",
        x = "Hour of Day",
        y = "Number of Trips")
  })
  
  output$trips_by_month <- renderPlot({
    dataset %>%
      count(month_name) %>%
      ggplot(aes(x = month_name, y = n, group = 1)) +  
      geom_line(color = "blue", size = 1) +
      labs(
        title = "Number of Uber Trips by Month",
        x = "Month",
        y = "Number of Trips")     
  })
  
  output$trips_by_day <- renderPlot({
    filtered_data() %>%
      count(date) %>%
      ggplot(aes(x = date, y = n, group = 1)) + 
      geom_line(color = "blue", size = 1) +
      labs(
        title = "Number of Uber Trips by Day",
        x = "Day of Month", 
        y = "Number of Trips")     
  })
  
  output$trips_by_day_month <- renderPlot({
    dataset %>%
      count(week_day, month_name) %>%
      ggplot(aes(x = month_name, y = n, fill = week_day)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      scale_fill_brewer(palette = "Set1") +
      labs(
        title = "Number of Uber Trips by Day of Week and Month",
        x = "Month",
        y = "Number of Trips",
        fill = "Day of Week")
  })
  
  output$trips_by_bases_month <- renderPlot({
    dataset %>%
      count(Base, month_name) %>%
      ggplot(aes(x = month_name, y = n, fill = Base)) +
      geom_bar(stat = 'identity', position = 'stack') +
      scale_fill_brewer(palette = 'Set1') +
      labs(
        title = 'Number of Uber Tips by Bases and Month',
        x = 'Month',
        y = 'Number of Trips',
        fill = 'Base'
      )
  })
  
  output$trips_by_hour_day <- renderPlot({
    dataset %>%
      count(hour, week_day) %>%
      ggplot(aes(x = hour, y = week_day, fill = n)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "darkblue") +
      labs(
        title = "Number of Trips by Hour and Day",
        x = "Hour of Day",
        y = "Day of Week",
        fill = "Number of Trips"
      )
  })
  
  output$trips_by_month_day <- renderPlot({
    dataset %>%
      count(month_name, week_day) %>%
      ggplot(aes(x = month_name, y = week_day, fill = n)) + 
      geom_tile() +
      scale_fill_gradient(low = "white", high = "darkblue") +
      labs(
        title = "Number of Trips by Month and Day",
        x = "Month",
        y = "Day of Week",
        fill = "Number of Trips")
  })
  
  output$trips_by_base_weekday <- renderPlot({
    dataset %>%
      count(Base, week_day) %>%
      ggplot(aes(x = Base, y = week_day, fill = n)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "darkblue") +
      labs(
        title = "Number of Trips by Base and Weekday",
        x = "Base",
        y = "Weekday",
        fill = "Number of Trips")
  })
  
  output$trips_by_hour_pivot <- renderDataTable({
    dataset %>%
      count(hour) %>%
      arrange(hour) %>%
      rename(`Hour` = hour, `Number of Trips` = n)
  })
  
  output$daily_trips <- renderDataTable({
    dataset %>%
      count(date) %>%
      arrange(date) %>%
      rename(`Day in Month` = date, `Number of Trips` = n) 
  })
  output$trip_map <- renderLeaflet({
    leaflet(data = dataset[1:500, ]) %>%
      addTiles() %>%
      addCircleMarkers(~Lon, ~Lat, radius = 2, color = "blue", fillOpacity = 0.5)
  })
  output$linear_plot <- renderPlot({
    linear_data <- filtered_data() %>% 
      count(week_day) %>%
      mutate(
        weekday_num = as.numeric(week_day)
      )
    
    model <- lm(n ~ weekday_num, data = linear_data)
    
    ggplot(linear_data, aes(x = week_day, y = n, group = 1)) +
      geom_point(color = 'blue', size = 3) + 
      geom_smooth(aes(x = weekday_num), method = 'lm', color = 'red') + 
      labs(title = 'Prediction Ride by Weekday using Linear Regression', 
           x = 'Day of Week', 
           y = 'Number of Trips') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui, server)
