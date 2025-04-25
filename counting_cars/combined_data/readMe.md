# Counting Cars Combined 🚗

## Minh Nguyen ☀️

## Introduction
This project is the next step of the Counting Cars project, which took all of my DATA-332 classmates' data, wrangled, cleaned and ingested them into my previous shiny app. This project focuses on data wrangling and data cleaning, following the tidy data guideline.

INTERACT WITH OUR SHINY APP [HERE](https://minhnguyen22.shinyapps.io/combined_cars/)!

## Data Dictionary 📖
Our dataset includes the following columns:

- **date**: recorded date
- **time**: recorded time
- **initial_speed**: first observed car speed
- **slow_down**: whether the car slow down after seeing the sign or not
- **body_type**: car style
- **vehicle_style**: Car style


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

# What I learned in general
- How to deploy a shiny app and the importance of file organization. I was struggling to deploy the app due to mismatch in my file path :))) It was painful
- Importance of planning ahead. Instead of blindly cleaning each file, I set up a clear data structure before hand following the tidy data guideline. This serves as a guiding map and save so much time!

# What I learned about collecting data
- Cannot trust people about data gathering :))) Even when we have talked and confirmed the structure before, they still made crazy changes. 
