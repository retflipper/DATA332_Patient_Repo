library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)

rm(list = ls()) #clears RStudio Environment

setwd('C:/Users/retai/Documents/r_projects/patient') #set working directory

df_billing <- read_excel('data/Billing.xlsx', .name_repair = 'universal')
df_patient <- read_excel('data/Patient.xlsx', .name_repair = 'universal')
df_visit <- read_excel('data/Visit.xlsx', .name_repair = 'universal')

df_visit_billing <- left_join(df_billing, df_visit, by = c('VisitID'))
df <- left_join(df_patient, df_visit_billing, by = c('PatientID'))

df[c('visit_year','visit_month', 'visit_day')] <- str_split_fixed(df$VisitDate, '-', 3)
df <- unite(df, city_state, City:State, sep = ", ")

df_visit_reasons <- df %>%
  group_by(Reason, visit_month, WalkIn, city_state) %>%
  summarize(count = n())

ggplot(df_visit_reasons, aes(x = Reason, y = count, fill = visit_month)) + 
  geom_col() +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(df_visit_reasons, aes(x = Reason, y = count, fill = WalkIn)) + 
  geom_col() +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1))


ggplot(df_visit_reasons, aes(x = city_state, y = count, fill = Reason)) + 
  geom_col() +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(df_visit_reasons, aes(x = Reason, y = count, fill = WalkIn)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1))

df_reason_cost <- df %>%
  group_by(Reason, InvoicePaid) %>%
  summarize(total_cost = sum(InvoiceAmt))

ggplot(df_reason_cost, aes(x = Reason, y = total_cost, fill = InvoicePaid)) + 
  geom_col() +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1))

df_reason_invoiceitem <- df %>%
  group_by(Reason, InvoiceItem) %>%
  summarize(count = n())

ggplot(df_reason_invoiceitem, aes(x = Reason, y = count, fill = InvoiceItem)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1))