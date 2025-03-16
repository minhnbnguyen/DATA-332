library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(here)

rm(list = ls())

setwd('~/Documents/r_projects/patient/data')

patient <- read_excel('Patient.xlsx', sheet = 1,.name_repair = 'universal')
billing <- read_excel('Billing.xlsx', sheet = 1,.name_repair = 'universal')
visit <- read_excel('Visit.xlsx', sheet = 1,.name_repair = 'universal')                      

#Join Patient & Billing
df_patient_visit <- left_join(visit, patient, by = c('PatientID'))
df_patient_bill <- inner_join(df_patient_visit,billing, by = c('VisitID'))

#Visit segmented (stacked bar chart) by month of the year. 
df_visit_segment <- df_patient_visit %>%
  group_by(visitMonth = month(VisitDate, label = TRUE), WalkIn) %>%
  summarize(patient_num = n())

ggplot(df_visit_segment, aes(fill = WalkIn, y = patient_num, x = visitMonth)) +
  geom_bar(position = "Stack", stat = "identity") +
  ggtitle("Reason for visit based on walk in or not")

#Reason for visit based on City/State or zip code
df_visit_segment <- df_patient_visit %>%
  group_by(cityState = paste(City, State, sep = " "), visitMonth = month(VisitDate, label = TRUE)) %>%
  summarize(patient_num = n())

ggplot(df_visit_segment, aes(fill = cityState, y = patient_num, x = visitMonth)) +
  geom_bar(position = "Stack", stat = "identity") +
  ggtitle("Reason for visit based on location")

#Total invoice amount based on reason for visit
#Segmented (stacked bar chart) with it was paid

df_visit_segment <- df_patient_bill %>%
  group_by(WalkIn, invoiceMonth = month(InvoiceDate, label = TRUE)) %>%
  summarize(patient_num = n())

ggplot(df_visit_segment, aes(fill = WalkIn, y = patient_num, x = invoiceMonth)) +
  geom_bar(position = "Stack", stat = "identity") +
  ggtitle("Total invoice amount based on reason for visit")

#my question: what generation pay the most for each visit?
df_patient_bill$birthYear <- as.numeric(format(df_patient_bill$BirthDate, "%Y"))

df_patient_bill$gen_bucket <- cut(df_patient_bill$birthYear,
                                  breaks = c(1937,1945, 1964, 1980, 1996, 2006),
                                  labels = c("silent gen","boomers", "X", "Y", "Z"),
                                  right = TRUE)
df_bill_segment <- df_patient_bill %>%
  group_by(gen_bucket, WalkIn) %>%
  summarize(patient_num = n())

ggplot(df_bill_segment, aes(fill = WalkIn, y = patient_num, x = gen_bucket)) +
  geom_bar(position = "Stack", stat = "identity") +
  ggtitle("Invoice Amount From Each Generation")

#older generation pay more for appointment visit, younger generation pay more for walk in