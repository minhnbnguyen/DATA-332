#Minh Nguyen
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
install.packages("viridis")
library(viridis)
install.packages("hrbrthemes")
library(hrbrthemes)
#resetting RStudio Environment
rm(list = ls())
#set working directory
setwd('~/Documents/r_projects/student')

df_course <- read_excel('Course.xlsx', .name_repair = 'universal')
df_registration <- read_excel('Registration.xlsx', .name_repair = 'universal')
df_student<- read_excel('Student.xlsx', .name_repair = 'universal')

#Left Join the data together and find insights in the data.
df <- left_join(df_registration, df_student, by = c('Student.ID'))
df <- left_join(df, df_course, by = c('Instance.ID'))

#Chart on the number of majors (TITLE) 
df_student_by_major <- df %>%
  group_by(Title) %>%
  summarize(student_num = n())

# Grouped
ggplot(df_student_by_major, aes(x = Title, y = student_num)) +
  geom_col(fill = "blue") +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1)) +
  ggtitle("Number of Majors")

#Chart on the birth year of the student
df$birth_year <- as.numeric(format(df$Birth.Date, "%Y"))

df_student_by_birth_year <- df %>%
  group_by(birth_year, Title) %>%
  summarize(student_num = n())

df_student_by_birth_year %>%
  ggplot( aes(x=birth_year, y=student_num)) +
  geom_line() +
  ggtitle("Student Birth Year")

#Chart on the generation of the student
#gen bucket
df_student_by_birth_year$gen_bucket <- cut(df_student_by_birth_year$birth_year,
                     breaks = c(1952, 1964, 1980, 1996, 2012),
                     labels = c("boomers", "X", "Y", "Z"),
                     right = TRUE)

ggplot(df_student_by_birth_year, aes(fill=Title, y=student_num, x=gen_bucket)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Student by generation")

#Total cost per major, segment by payment plan
df_cost <- df %>%
  group_by(Title, Payment.Plan) %>%
  summarize(student_num = n(),
            total_cost = sum(Total.Cost))

ggplot(df_cost, aes(fill=Payment.Plan, y=total_cost, x=Title)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Total Cost")

#Total balance due by major, segment by payment plan 
df_balance_due <- df %>%
  group_by(Title, Payment.Plan) %>%
  summarize(student_num = n(),
            total_due = sum(Balance.Due))

ggplot(df_balance_due, aes(fill=Payment.Plan, y=total_due, x=Title)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Total Balance Due")
