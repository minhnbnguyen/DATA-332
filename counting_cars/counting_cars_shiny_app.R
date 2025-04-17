library(shiny)
library(ggplot2)
library(readxl)
library(lubridate)
library(janitor)
library(DT)
library(RCurl)
library(dplyr)

# Read data
data_url <- data_url <- getURL('https://raw.githubusercontent.com/nickhc41703/Data_332_assignments/main/Homework/counting_cars/counting_cars_final.csv')
dataset <- read.csv(text = data_url)

# Prep data
dataset <- dataset %>%
  mutate(mph_group = cut(
    mph,
    breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40),
    labels = c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40"),
    include.lowest = TRUE,
    right = TRUE
  ))

# count -> x variable
brand_count <- dataset %>% count(brand, name = "count")
style_count <- dataset %>% count(vehicle_style, name = "count")
speed_count <- dataset %>% count(mph_group, if_they_slow_down_.YES..NO., name = "count")

# UI set up
ui<-fluidPage( 
  
  titlePanel(title = "Explore Counting Cars"),
  h4('Nick Camacho, Zoey Do, Minh Nguyen'),
  
  fluidRow(
    column(12,
           h5("Summary Statistics"),
           tableOutput('summaryTable'),
           br()
    )
  ),
  
  fluidRow(
    column(2,
           selectInput('X', 'Choose X',choices=c("brand","vehicle_style","speed"),selected="brand")),
    column(10,plotOutput('plot_01'),br(),htmlOutput("analysis"))
  ))

# Set up server
server<-function(input,output) {
  data <- reactive({
    if(input$X == "brand") {
      return(brand_count)
    } else if(input$X == "vehicle_style"){
      return(style_count)
    } else {
      return(speed_count)
    }
  })
  # Main plot output
  output$plot_01 <- renderPlot({
    if(input$X == "speed") {
      # Special handling for speed graph with stacked bars
      ggplot(speed_count, aes(x = mph_group, y = count, fill = if_they_slow_down_.YES..NO.)) +
        geom_bar(stat = "identity", position = "stack") +
        labs(x = "MPH Group", y = "#Cars", fill = "Slowed Down?") +
        scale_fill_manual(values = c("yes" = "blue", "no" = "grey")) +
        theme_minimal() +
        theme(
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 12)
        )
    } else {
      # Standard bar chart for brand or vehicle_style
      ggplot(data(), aes(x = reorder(get(input$X), -count), y = count, fill = count)) +
        geom_bar(stat = "identity") +
        scale_fill_gradient(low = "grey", high = "blue") +
        labs(x = input$X, y = "#Cars") +
        theme_minimal() +
        theme(
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 12)
        )
    }
  })
  output$summaryTable <- renderTable({
    dataset %>%
      summarise(
        Min_Speed = min(mph, na.rm = TRUE),
        Max_Speed = max(mph, na.rm = TRUE),
        Mean_Speed = round(mean(mph, na.rm = TRUE), 2)
      )
  })
  output$analysis <- renderUI({
    text <- if (input$X == "speed") {
      "We decided to make a stacked bar chart and group the mph by every 5 miles for the x value and get the count of cars for the y value so that we can get a range of what mph range is most likely to slow down. The speed range that slowed down the most was 16 - 20 mph. It is important to note that the speed limit for that spot was 30 mph, and there were no cars that slowed down if they were going past the 30 mph speed limit."
    } else if (input$X == "vehicle_style") {
      "The chart shows that <b>SUVs</b> are the most common vehicle type with <b>121 cars</b>, followed by <b>sedans (67)</b> and <b>pickup trucks (24)</b>. The chart shows that SUVs are the most common vehicle type with 121 cars, followed by sedans (67) and pickup trucks (24). Other styles like hatchbacks, bugs, and coupes the least, appear 8 times in total. This suggests SUVs and sedans make up the majority of vehicles observed in the area."
    } else {
      "The chart shows <b>Ford</b> is the most frequently observed brand with <b>42 cars</b>, followed by <b>Chevrolet (31)</b> and <b>Honda (21)</b>.The chart shows that SUVs are the most common vehicle type with 121 cars, followed by sedans (67) and pickup trucks (24). Other styles like hatchbacks, bugs, and coupes the least, appear 8 times in total. This suggests SUVs and sedans make up the majority of vehicles observed in the area."
    }
    
    HTML(paste0("<div style='font-size:16px;'>", text, "</div>"))
  })
}  
shinyApp(ui=ui, server=server)