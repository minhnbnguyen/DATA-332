# Explore Counting Cars 🚗

## By Nick Camacho, Zoey Do, Minh Nguyen ☀️

## Introduction
This project explores if the speed radar is effective or not. Speed radar catches the car speed. Our goal of the project is to see if the driver slow down after seeing their speed or not. We also want to analyze the brand and vehicle style associate with the speed

INTERACT WITH OUR SHINY APP [HERE](https://minhnguyen22.shinyapps.io/counting_cars/)!


## How we gather the data
- We went 3 different places that have speed radars on different days and recorded the car brand, vehicle style, starting speed, if they slow down or not.
- We also include the date and time.

# How our team communicate
- We communicate daily via Snapchat.
- We also use Gmail to send our working R file and analysis to share with each other.
- We also met with each other twice outside of class to go over the project and the presentation.

## Data Dictionary 📖
Our dataset includes the following columns:

- **primary_key**: Car Order
- **student**: Student who record this car
- **date**: Recorded date
- **mph**: First observed car speed
- **brand**: Car brand
- **vehicle_style**: Car style
- **hr:min**: Specific hour recorded
- **if_they_slow_down_(YES/ NO)**: Record if the slow down after seeing the sign or not


## Data Summary
- There are total 225 cars, each student recorded 75 cars
- The lowest speed recorded is 10 miles/h
- The highest speed recorded is 38 miles/h
- The average speed recorded is 24.72 miles/h

## Key Findings

### Car Brand Analysis
![Word Cloud](https://github.com/minhnbnguyen/DATA-332/blob/main/counting_cars/graphs/car_brand.png)
- The chart shows Ford is the most frequently observed brand with 42 cars, followed by Chevrolet (31) and Honda (21).
- Brands like Lexus, Pontiac, and Prius appear the least, with 3 times in total.
- This indicates that Ford and Chevrolet dominate the traffic in the observed area.



### Vehicle Style Analysis
![Net Sentiment](https://github.com/minhnbnguyen/DATA-332/blob/main/counting_cars/graphs/vehicle_style.png)
- The chart shows that SUVs are the most common vehicle type with 121 cars, followed by sedans (67) and pickup trucks (24).
- Other styles like hatchbacks, bugs, and coupes the least, appear 8 times in total.
- This suggests SUVs and sedans make up the majority of vehicles observed in the area.



### Speed Analysis
![Emotional content](https://github.com/minhnbnguyen/DATA-332/blob/main/counting_cars/graphs/if_slow_down.png)
- We decided to make a stacked bar chart and group the mph by every 5 miles for the x value and get the count of cars for the y value so that we can get a range of what mph range is most likely to slow down.
- The speed range that slowed down the most was 16 - 20 mph.
- It is important to note that the speed limit for that spot was 30 mph, and there were no cars that slowed down if they were going past the 30 mph speed limit.
