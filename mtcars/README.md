# Explore Counting Cars 🏦

## By Nick Camacho, Zoey Do, Minh Nguyen ☀️

## Introduction
- bla bla

## How we gather the data
- bla bla

# How our team communicate
- Via Snapchat and Gmail

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
![Word Cloud](https://github.com/minhnbnguyen/DATA-332/blob/main/Chase_Complaint_Analysis/visualization/wordcloud.png)
- The chart shows Ford is the most frequently observed brand with 42 cars, followed by Chevrolet (31) and Honda (21).
- Brands like Lexus, Pontiac, and Prius appear the least, with 3 times in total.
- This indicates that Ford and Chevrolet dominate the traffic in the observed area.



### Vehicle Style Analysis
![Net Sentiment](https://github.com/minhnbnguyen/DATA-332/blob/main/Chase_Complaint_Analysis/visualization/netsentiment.png)
- The chart shows that SUVs are the most common vehicle type with 121 cars, followed by sedans (67) and pickup trucks (24).
- Other styles like hatchbacks, bugs, and coupes the least, appear 8 times in total.
- This suggests SUVs and sedans make up the majority of vehicles observed in the area.



### Speed Analysis
![Emotional content](https://github.com/minhnbnguyen/DATA-332/blob/main/Chase_Complaint_Analysis/visualization/disputevsnondispute_emotion.png)
- We decided to make a stacked bar chart and group the mph by every 5 miles for the x value and get the count of cars for the y value so that we can get a range of what mph range is most likely to slow down.
- The speed range that slowed down the most was 16 - 20 mph.
- It is important to note that the speed limit for that spot was 30 mph, and there were no cars that slowed down if they were going past the 30 mph speed limit.


