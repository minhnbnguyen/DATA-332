#Minh Nguyen
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
#resetting RStudio Environment
rm(list = ls())
#set working directory
setwd('~/Documents/r_projects/trucking')

df_truck_0001 <- read_excel('truck data 0001.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_0369 <- read_excel('truck data 0369.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1226<- read_excel('truck data 1226.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1442 <- read_excel('truck data 1442.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1478 <- read_excel('truck data 1478.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1539 <- read_excel('truck data 1539.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1769 <- read_excel('truck data 1769.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_pay <- read_excel('Driver Pay Sheet.xlsx', .name_repair = 'universal')

df <- rbind(df_truck_0001,df_truck_0369,df_truck_1226,
            df_truck_1442,df_truck_1478,df_truck_1539,
            df_truck_1769)

df <- df[,!(names(df) %in% c("...10"))]

df_starting_pivot <- df %>%
  group_by(Truck.ID) %>%
  summarize(count = n())

df_truck <- left_join(df, df_pay, by = c('Truck.ID'))

df <- df_truck[, c(4:19)]

#visualization
#Starting warehouse
df[c('warehouse','starting_city_state')] <-
  str_split_fixed(df$Starting.Location, ",", 2)

#start by just completing the group by with count, then add by using
df_starting_pivot <- df %>%
  group_by(starting_city_state) %>%
  summarize(count = n(),
            mean_size_hours = mean(Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm = TRUE),
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
  )

#Start column by just those columns then add complexivity
ggplot(df_starting_pivot, aes(x = starting_city_state, y = count)) +
  geom_col() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))

#Do again with delivery location
df[c('recipient', 'delivery_city_state')] <-
  str_split_fixed(df$Delivery.Location, ",", 2)

df_delivery_pivot <- df %>%
  group_by(delivery_city_state) %>%
  summarize(count = n(),
            mean_size_hours = mean(Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm = TRUE),
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
  )

ggplot(df_delivery_pivot, aes(x = delivery_city_state, y = count)) +
  geom_col() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))

#Who has the highest pay?
#pay
df$driver_fullname <- paste(df$first, df$last, sep=" ")

df_driver_pay <- df %>%
  group_by(driver_fullname) %>%
  summarize(
    total_pay = sum((df$Odometer.Ending - df$Odometer.Beginning)*labor_per_mil)
  )

ggplot(df_driver_pay, aes(x = driver_fullname, y = total_pay)) +
  geom_col() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))
#Bill Burr gets paid the most
