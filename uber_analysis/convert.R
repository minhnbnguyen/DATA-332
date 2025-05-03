library(readr)
library(dplyr)
library(lubridate)

rm(list = ls())

# Read excel data
setwd('~/Documents/r_projects/uber')

apr14 <- read.csv('uber-raw-data-apr14.csv')
aug14 <- read.csv('uber-raw-data-aug14.csv')
jul14 <- read.csv('uber-raw-data-jul14.csv')
jun14 <- read.csv('uber-raw-data-jun14.csv')
may14 <- read.csv('uber-raw-data-may14.csv')
sep14 <- read.csv('uber-raw-data-sep14.csv')

# Save each file as RDS with compression
saveRDS(apr14, 'data/apr14.rds', compress = TRUE)
saveRDS(aug14, 'data/aug14.rds', compress = TRUE)
saveRDS(jul14, 'data/jul14.rds', compress = TRUE)
saveRDS(jun14, 'data/jun14.rds', compress = TRUE)
saveRDS(may14, 'data/may14.rds', compress = TRUE)
saveRDS(sep14, 'data/sep14.rds', compress = TRUE)

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

# Check file sizes to verify compression
orig_size <- file.size('uber-raw-data-apr14.csv') + 
  file.size('uber-raw-data-aug14.csv') +
  file.size('uber-raw-data-jul14.csv') + 
  file.size('uber-raw-data-jun14.csv') +
  file.size('uber-raw-data-may14.csv') + 
  file.size('uber-raw-data-sep14.csv')

new_size <- file.size('data/uber_dataset.rds')

message("Original CSV size: ", orig_size/1024/1024, " MB")
message("New RDS size: ", new_size/1024/1024, " MB")
message("Compression ratio: ", round(new_size/orig_size * 100, 2), "%")
