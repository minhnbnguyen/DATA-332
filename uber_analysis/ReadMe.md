# UBER TRIPS ANALYSIS 🚗

## Minh Nguyen ☀️

## Introduction
This project analyzes the Uber Trip Data in NYC from April to September 2014.

INTERACT WITH MY SHINY APP [HERE](https://minhnguyen22.shinyapps.io/uber_analysis/)!

## Data Cleaning 🧹
Due to the large size of csv files, I couldn't deploy it to Shiny free version. Thus I compress csv files into 1 rds file to reduce size. I then perform data cleaning in that rds datafile also.
```r
# Read excel data
setwd('~/Documents/r_projects/uber')
# Read each CSV file
apr14 <- read.csv('uber-raw-data-apr14.csv')
aug14 <- read.csv('uber-raw-data-aug14.csv')
...

# Save each file as RDS with compression
saveRDS(apr14, 'data/apr14.rds', compress = TRUE)
saveRDS(aug14, 'data/aug14.rds', compress = TRUE)
...

# Combine all data and save as one RDS file
dataset <- rbind(apr14, aug14, jul14, jun14, may14, sep14)

# Clean and preprocess the data
dataset <- dataset %>%
  mutate(
    Date.Time = mdy_hms(Date.Time),
    date = day(Date.Time),
    hour = format(Date.Time, "%H"),
    month = format(Date.Time, "%m"),
    month_name = month(Date.Time, label = TRUE, abbr = TRUE),
    week_day = wday(Date.Time, label = TRUE, abbr = TRUE)
  )

# Save the combined preprocessed dataset
saveRDS(dataset, 'data/uber_dataset.rds', compress = TRUE)
```

## Line Graph Sample Code
```r
  output$trips_by_month <- renderPlot({
    dataset %>%
      count(month_name) %>%
      ggplot(aes(x = month_name, y = n, group = 1)) +  # Using month_name instead of month
      geom_line(color = "blue", size = 1) +
      labs(
        title = "Number of Uber Trips by Month",
        x = "Month",
        y = "Number of Trips")     
  })
```
## Bar Graph Sample Code
I stole the code from their Shiny app and edit it
```r
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
```
## Heat Map Sample Code
```r
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
```
## Pivot Table Sample Code
```r
  output$trips_by_hour_pivot <- renderDataTable({
    dataset %>%
      count(hour) %>%
      arrange(hour) %>%
      rename(`Hour` = hour, `Number of Trips` = n)
  })
```
### Leaflet Map Code
```r
  output$trip_map <- renderLeaflet({
    leaflet(data = dataset[1:500, ]) %>%
      addTiles() %>%
      addCircleMarkers(~Lon, ~Lat, radius = 2, color = "blue", fillOpacity = 0.5)
  })
```
### Leaflet Map Code
```r
  output$trip_map <- renderLeaflet({
    leaflet(data = dataset[1:500, ]) %>%
      addTiles() %>%
      addCircleMarkers(~Lon, ~Lat, radius = 2, color = "blue", fillOpacity = 0.5)
  })
```
### Linear Regression Prediction Model
```r
  output$linear_plot <- renderPlot({
    # Count trips by weekday
    linear_data <- filtered_data() %>% 
      count(week_day) %>%
      # Convert weekday to a factor with levels in correct order (Sun to Sat)
      mutate(
        # First convert to proper weekday order
        weekday_num = as.numeric(week_day)
      )
    
    # Build linear model based on weekday
    model <- lm(n ~ weekday_num, data = linear_data)
    
    # Create prediction plot
    ggplot(linear_data, aes(x = week_day, y = n, group = 1)) +
      geom_point(color = 'blue', size = 3) + 
      geom_smooth(aes(x = weekday_num), method = 'lm', color = 'red') + 
      labs(title = 'Prediction Ride by Weekday using Linear Regression', 
           x = 'Day of Week', 
           y = 'Number of Trips') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
```
