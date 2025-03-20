library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(tidyr)
library(readr)

# set up environment
rm(list = ls())
setwd('~/Documents/r_projects/text_analysis/data')

# read file into tibble
complaints_tibble <- read.csv('Consumer_Complaints.csv')

# I. data cleaning
# a. convert data type
complaints_tibble$Date.received <- as.Date(complaints_tibble$Date.receive, format = '%m/%d/%Y')
complaints_tibble$Date.sent.to.company <- as.Date(complaints_tibble$Date.sent.to.company, format = '%m/%d/%Y')

# b. tag col include multiple variable -> split those with multiple selections into separated rows
complaints_tibble <- complaints_tibble %>%
  separate_rows(Tags, sep = ", ")

# c. convert all empty cells into N/A instead of ""
col_w_empty_cells <- names(complaints_tibble[colSums(complaints_tibble == "", na.rm = TRUE) > 0])

complaints_tibble <- complaints_tibble %>%
  mutate(across(col_w_empty_cells, ~na_if(., "")))

# d. convert N/A to na
na_cols_logical <- sapply(complaints_tibble, function(x) {
  if(is.character(x)) {
    return(any(x == "N/A", na.rm = TRUE))
  } else {
    return(FALSE)
  }
})

cols_with_NA_text <- names(na_cols_logical)[na_cols_logical]

complaints_tibble <- complaints_tibble %>%
  mutate(across(cols_with_NA_text, ~na_if(., "N/A")))
