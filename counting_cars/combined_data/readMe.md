# Counting Cars Combined 🚗

## Minh Nguyen ☀️

## Introduction
This project is the next step of the Counting Cars project, which took all of my DATA-332 classmates' data, wrangled, cleaned and ingested them into my previous shiny app. This project focuses on data wrangling and data cleaning, following the tidy data guideline.

INTERACT WITH MY SHINY APP [HERE](https://minhnguyen22.shinyapps.io/combined_cars/)!

## Data Source
1. NBasil and co [HERE](https://github.com/rohaanfarrukh/data332_counting_cars/blob/main/counting_cars_project/rscript/speed_counting_cars.xlsx)
2. Tommy  and co [HERE](https://github.com/TommyAnderson/Car-Data-Analysis/blob/main/Car%20Data%20Collection.csv)
3. Nick and co [HERE](https://github.com/nickhc41703/Data_332_assignments/blob/main/Homework/counting_cars/counting_cars_final.csv)
4. Ahbid and co [HERE](https://github.com/kritansth/data332/blob/main/counting_cars/cars_count.xlsx)
5. Tanner and co [HERE](https://github.com/retflipper/DATA332_CountingCars/blob/main/data/Counting_Cars.csv)
6. Nisrine and co [HERE](https://github.com/nissou62/The-very-basics-of-R/blob/main/shinymtcar_project/Data_Counting_Cars.csv)
7. x [HERE](https://github.com/1R0NCL4D-B4ST10N/DATA332/blob/361329a6e87b930e66e87f20f2d137f2f0810a46/carTracker/carTracker.xlsx)

## Data Dictionary 📖
Our dataset includes the following columns:

- **date**: recorded date
- **time**: recorded time
- **initial_speed**: first observed car speed
- **slow_down**: whether the car slow down after seeing the sign or not
- **body_type**: car style

## My Cleaning Process 🧹

### Create a lookup car type table for Tanner.
I stole the code from their Shiny app and edit it
```r
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
#Join that table with type of car
tanner_joined <- left_join(tanner_data, tanner_car_lookup, by = c("Type_of_Car" = "number"))
```
### Put into a clean dataframe following the structure in the data dictionary
```r
# Ensure data type as date
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

```
### Repeat the process for other datasource (adjust accordingly) and bind them together
```r
dataset <- rbind(ahbid_data_clean,
                    nbasil_data_clean,
                    nick_data_clean,
                    nsrine_data_clean,
                    tanner_data_clean,
                    tommy_data_clean,
                    x_data_clean)
```
### Ensure consistency in union dataset
```r
unique_body_type <- unique(dataset$body_type)
print(unique_body_type)

dataset$body_type <- recode(dataset$body_type,
                               "sevan" = "sedan",
                               "suv " = "suv",
                               "pickup_truck" = "pickup truck",
                               "muscle_car" = "muscle car",
                               "van " = "van",
                               "van_" = "van")
# Check category again
unique_body_type <- unique(dataset$body_type)
print(unique_body_type)
```

## Data Summary
- The lowest speed recorded is 10 miles/h
- The highest speed recorded is 44 miles/h
- The average speed recorded is 29.24 miles/h

## Key Findings

### Time Analysis
![Word Cloud](https://github.com/minhnbnguyen/DATA-332/blob/main/counting_cars/graphs/car_brand.png)
- The chart shows Afternon is the most frequently observed time with more than 600 cars, followed by Morning and Evening.
- This either suggests that students are mostly free in the afternoon to observe car, or that afternoon has more cars on the road.
- Either way, this is just a brief conclusion, with further study including larger random sample needed.



### Vehicle Style Analysis
![Net Sentiment](https://github.com/minhnbnguyen/DATA-332/blob/main/counting_cars/combined_data/graphs/body_type.png)
- The chart shows that SUVs are the most common vehicle type with nearly 400 cars, followed by sedans (300) and pickup trucks/trucks (120).
- Other styles like hatchbacks, bugs, and coupes the least.
- This suggests SUVs and sedans make up the majority of vehicles observed in the area.



### Speed Analysis
![Emotional content](https://github.com/minhnbnguyen/DATA-332/blob/main/counting_cars/combined_data/graphs/time_of_day.png)
- We decided to make a stacked bar chart and group the mph by every 5 miles for the x value and get the count of cars for the y value
- Therefore, we can get a range of what mph range is most likely to slow down.
- The speed range that slowed down the most was 26-30 mph.
- It is important to note that the speed limit for that spot was 30 mph.

## Reflection

### What I learned in general
- How to deploy a shiny app and the importance of file organization. I was struggling to deploy the app due to mismatch in my file path. It was painful 🥲.
- Importance of planning ahead. Instead of blindly cleaning each file, I set up a clear data structure before hand following the tidy data guideline. This serves as a guiding map and save so much time!

### What I learned about collecting data
- Cannot trust people about data gathering 😤 Even when we have talked and confirmed the structure before, they still made crazy changes. 
- For example, even though we all agreed to have at least the time/date column, a lot of group doesn't have it. They also don't record enough 225 cars.
- Moreover, the way we format time and date are all different, thus it has to be managed independently
