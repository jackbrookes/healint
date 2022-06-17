library(ggplot2)
library(readr)
library(tidyverse)
library(ggprism)
library(rworldmap)
library(tibbletime)
library(lubridate)
library(dplyr)
library(countrycode)
library(ggprism)
library(hms)

# Read in migraine data

migraine_data <- read_csv("Data/migraines_base.csv") %>% 
  as_tbl_time(index = migraine_starttime_local)

# Extract month of start time

migraine_data$month <- month(migraine_data$migraine_starttime_local)

migraine_frequency <- migraine_data %>% 
  group_by(hashed_userid, month) %>% 
  tally() %>% 
  complete(hashed_userid, month, fill = list(n = 0)) %>% 
  group_by(hashed_userid) %>% 
  summarise(attacks_per_month = mean(n), round(ceiling(attacks_per_month))) %>%
  
  group_by(hashed_userid) %>% 
  rename(attacks_per_month_round = `round(ceiling(attacks_per_month))`) %>% 
  mutate(frequency = case_when(attacks_per_month_round >= 8 && attacks_per_month <= 14 ~ "HFEM", 
                               attacks_per_month_round >=4 && attacks_per_month <=7 ~ "LFEM",
                               attacks_per_month_round >=15 ~ "CM", 
                               attacks_per_month_round >=0 && attacks_per_month <= 3 ~ "VLFEM")) 
# Count length of attacks

migraine_data$hours_of_attack <- with(migraine_data, difftime(migraine_endtime_local,migraine_starttime_local, units="hours"))

migraine_data <- migraine_data %>% 
  group_by(hashed_userid) %>% 
  mutate(mean_attack_length = mean(hours_of_attack))