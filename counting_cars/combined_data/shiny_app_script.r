library(readxl)
library(lubridate)
library(janitor)
library(DT)
library(RCurl)
library(dplyr)
library(shiny)
library(ggplot2)

#rm(list = ls())
# DATA WRANGLING

# Read csv data
nick_url <- data_url <- getURL('https://raw.githubusercontent.com/nickhc41703/Data_332_assignments/main/Homework/counting_cars/counting_cars_final.csv')
nick_data <- read.csv(text = nick_url)

tommy_url <- data_url <- getURL('https://raw.githubusercontent.com/TommyAnderson/Car-Data-Analysis/main/Car%20Data%20Collection.csv')
tommy_data <- read.csv(text = tommy_url)

tanner_url <- data_url <- getURL('https://raw.githubusercontent.com/retflipper/DATA332_CountingCars/main/data/Counting_Cars.csv')
tanner_data <- read.csv(text = tanner_url)

nsrine_url <- data_url <- getURL('https://raw.githubusercontent.com/nissou62/The-very-basics-of-R/main/shinymtcar_project/Data_Counting_Cars.csv')
nsrine_data <- read.csv(text = nsrine_url)

# Read excel data
# Since excel data is not in text, the url reading approach don't support this. Thus I do the manual download approach for these files
#setwd('~/Documents/r_projects/counting_cars/data')
nbasil_data <- read_excel('speed_counting_cars.xlsx', .name_repair = 'universal')
ahbid_data <- read_excel('cars_count.xlsx', .name_repair = 'universal')
x_data <- read_excel('carTracker.xlsx', .name_repair = 'universal')

# Standard data structure: Date (MM/DD/YYYY), Time (military), initial speed, slow down/not (yes/no), body(car) type
# Clean x_data
x_data$TimeTracked <- as.POSIXct(x_data$TimeTracked, format="%Y-%m-%d %H:%M:%S")

x_data_clean <- data.frame(
  date = format(x_data$TimeTracked, "%m/%d/%Y"),
  time = format(x_data$TimeTracked, "%H:%M"),
  initial_speed = x_data$MPH,
  slow_down = ifelse(x_data$Slow == "Y", "yes",
                     ifelse(x_data$Slow == "N","no", x_data$Slow)),
  body_type = NaN
)
# Clean ahbid_data
ahbid_data_clean <- data.frame(
  date = NaN,
  time = NaN,
  initial_speed = ahbid_data$Initial_Speed,
  slow_down = ifelse(ahbid_data$Difference < 1, "no",
                     ifelse(ahbid_data$Difference == 1, "yes",
                     ifelse(ahbid_data$Difference > 1, "yes", ahbid_data$Difference))),
  body_type = tolower(ahbid_data$Body_Style)
)
# Clean nsrine
nsrine_data$Date <- as.Date(nsrine_data$Date, format = "%A, %B %d, %Y")
nsrine_data$Time <- as.POSIXct(nsrine_data$Time, format = "%H:%M:%S")
nsrine_data_clean <- data.frame(
  date = format(nsrine_data$Date, "%m/%d/%Y"),
  time = format(nsrine_data$Time, "%H:%M"),
  initial_speed = nsrine_data$Speed..mph.,
  slow_down = NaN,
  body_type = NaN
)
# Clean tommy
tommy_data$Time.of.the.day <- gsub("(\\d+:\\d+): ([AP]M)", "\\1 \\2", tommy_data$Time.of.the.day)
times <- strptime(tommy_data$Time.of.the.day, format="%I:%M %p")
tommy_data$Time.of.the.day <- format(times, "%H:%M")
tommy_data$Date <- as.POSIXct(tommy_data$Date, format="%m/%d/%Y")

tommy_data_clean <- data.frame(
  date = format(tommy_data$Date, "%m/%d/%Y"),
  time = tommy_data$Time.of.the.day,
  initial_speed = tommy_data$Speed,
  slow_down = NaN,
  body_type = tolower(tommy_data$Type.of.Car)
)
# Clean tanner
# Create a lookup car type table for Tanner. I stole the code from their Shiny app and edit it
car_types <- c(
  "1" = "Emergency",
  "2" = "Hatchback",
  "3" = "Sedan",
  "4" = "SUV",
  "5" = "Van",
  "6" = "Minivan",
  "7" = "Motorcycle",
  "8" = "Coupe",
  "9" = "Truck",
  "10" = "Pickup Truck"
)

tanner_car_lookup <- data.frame(
  number = as.integer(names(car_types)),
  car_type = tolower(as.character(car_types)),
  stringsAsFactors = FALSE
)
# Join that table with type of car
tanner_joined <- left_join(tanner_data, tanner_car_lookup, by = c("Type_of_Car" = "number"))
# Put into final clean data
tanner_data$Date_Recorded <- as.POSIXct(tanner_data$Date_Recorded, format="%m/%d/%Y")
tanner_data$Time_Recorded <- as.POSIXct(tanner_data$Time_Recorded, format = "%H:%M:%S")

tanner_data_clean <- data.frame(
  date = format(tanner_data$Date_Recorded, "%m/%d/%Y"),
  time = format(tanner_data$Time_Recorded, "%H:%M"),
  initial_speed = tanner_data$Initial_Read,
  slow_down = ifelse(tanner_data$Difference_In_Readings < 1, "no",
                     ifelse(tanner_data$Difference_In_Readings == 1, "yes",
                            ifelse(tanner_data$Difference_In_Readings > 1, "yes", tanner_data$Difference_In_Readings))),
  body_type = tanner_joined$car_type
)

# Clean nbsil
nbasil_data_clean <- data.frame(
  date = NaN,
  time = NaN,
  initial_speed = nbasil_data$init_speed,
  slow_down = ifelse(nbasil_data$final_speed - nbasil_data$init_speed < 1, "no",
                      ifelse(nbasil_data$final_speed - nbasil_data$init_speed >= 1, "yes",
                             NA)),
  body_type = tolower(nbasil_data$vehicle_type)
)

# Clean Nick
nick_data$date <- as.POSIXct(nick_data$date, format="%m/%d/%Y")
nick_data$hr.min <- as.POSIXct(nick_data$hr.min, format = "%H:%M")

nick_data_clean <- data.frame(
  date = format(nick_data$date, "%m/%d/%Y"),
  time = format(nick_data$hr.min, "%H:%M"),
  initial_speed = nick_data$mph,
  slow_down = nick_data$if_they_slow_down_.YES..NO.,
  body_type = nick_data$vehicle_style
)

# Union all data
dataset <- rbind(ahbid_data_clean,
                    nbasil_data_clean,
                    nick_data_clean,
                    nsrine_data_clean,
                    tanner_data_clean,
                    tommy_data_clean,
                    x_data_clean)
# Clean body type
unique_body_type <- unique(dataset$body_type)
print(unique_body_type)

dataset$body_type <- recode(dataset$body_type,
                               "sevan" = "sedan",
                               "suv " = "suv",
                               "pickup_truck" = "pickup truck",
                               "muscle_car" = "muscle car",
                               "van " = "van",
                               "van_" = "van")

#write.csv(union_data,"/Users/apple/Documents/r_projects/counting_cars/data/union_clean.csv", row.names = FALSE) 
#print ('CSV created Successfully :)')

# SHINY APP SET UP
# Prep data
dataset <- dataset %>%
  mutate(speed_group = cut(
    initial_speed,
    breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40),
    labels = c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40"),
    include.lowest = TRUE,
    right = TRUE
  ))

dataset <- dataset %>%
  mutate(time_obj = hm(time))

dataset <- dataset %>%
  mutate(time_of_day = cut(
    hour(time_obj),
    breaks = c(0, 6, 12, 18, 24),
    labels = c("Night","Morning","Afternoon","Evening"),
    include.lowest = TRUE,
    right = FALSE
  ))

# count -> x variable
time_count <- dataset %>% count(time_of_day, name = "count")
style_count <- dataset %>% count(body_type, name = "count")
speed_count <- dataset %>% count(speed_group, slow_down, name = "count")

# UI set up
ui <- fluidPage( 
  
  titlePanel(title = "Explore Counting Cars"),
  h4('Minh Nguyen'),
  
  fluidRow(
    column(12,
           h5("Summary Statistics"),
           tableOutput('summaryTable'),
           br()
    )
  ),
  
  fluidRow(
    column(2,
           selectInput('X', 'Choose X',choices=c("time_of_day","body_type","initial_speed"),selected="time_of_day")),
    column(10,plotOutput('plot_01'),br(),htmlOutput("analysis"))
  ))

# Set up server
server <- function(input,output) {
  data <- reactive({
    if(input$X == "time_of_day") {
      return(time_count)
    } else if(input$X == "body_type"){
      return(style_count)
    } else {
      return(speed_count)
    }
  })
  # Main plot output
  output$plot_01 <- renderPlot({
    if(input$X == "initial_speed") {
      # Special handling for speed graph with stacked bars
      ggplot(speed_count, aes(x = speed_group, y = count, fill = slow_down, label = count)) +
        geom_bar(stat = "identity", position = "stack") + 
        labs(x = "Speed (mph)", y = "#Cars", fill = "Slowed Down?") +
        scale_fill_brewer() +
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
      ggplot(data(), aes(x = reorder(get(input$X), -count), y = count, fill = count, label = count)) +
        geom_bar(stat = "identity") +
        #scale_fill_brewer() +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
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
        Min_Speed = min(initial_speed, na.rm = TRUE),
        Max_Speed = max(initial_speed, na.rm = TRUE),
        Mean_Speed = round(mean(initial_speed, na.rm = TRUE), 2)
      )
  })
  
  output$analysis <- renderUI({
    text <- if (input$X == "initial_speed") {
      "We decided to make a stacked bar chart and group the mph by every 5 miles for the x value and get the count of cars for the y value so that we can get a range of what mph range is most likely to slow down. The speed range that slowed down the most was <b>26-30 mph</b>. It is important to note that the speed limit for that spot was 30 mph."
    } else if (input$X == "body_type") {
      "The chart shows that <b>SUVs</b> are the most common vehicle type with <b>nearly 400 cars</b>, followed by <b>sedans (300)</b> and <b>pickup trucks/trucks (120)</b>.Other styles like hatchbacks, bugs, and coupes the least. This suggests SUVs and sedans make up the majority of vehicles observed in the area."
    } else {
      "The chart shows <b>Afternon</b> is the most frequently observed time with <b> more than 600 cars</b>, followed by <b>Morning</b> and <b>Evening</b>. This either suggests that students are mostly free in the afternoon to observe car, or that afternoon has more cars on the road. Either way, this is just a brief conclusion, with further study including larger random sample needed."
    }
    
    HTML(paste0("<div style='font-size:16px;'>", text, "</div>"))
  })
}

shinyApp(ui=ui, server=server)
