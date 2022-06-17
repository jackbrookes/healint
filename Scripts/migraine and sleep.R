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


# join hours slept with migraine frequency data

sleep_migraine_data <- left_join(eliminated_sleep, migraine_frequency, by = "hashed_userid") %>% 
  na.omit() %>% 
  filter(attacks_per_month_round <= 29 | attacks_per_month_round == 0)

# join hours slept with migraine number of hours

sleep_migraine_duration <- left_join(migraine_data, by = "hasheduserid")


# plot frequency across mean hours slept

ggplot(sleep_migraine_data, aes(x = frequency, y = grand_mean_sleep, fill = frequency))+
  geom_violin(trim = FALSE)+
  geom_point(position = position_jitterdodge(0.48), alpha = 0.2)+ 
  ylab("Mean Hours Sleep Per Night")+
  xlab("Migraine Frequency")+
  theme_prism()+
  theme(legend.position = "top")


ggsave("Figures/frequency_sleep_violin.png", width = 5.5, height = 5, units = "in", type = "cairo")

# plot attacks against sleep

ggplot(sleep_migraine_data, aes(x = attacks_per_month_round, y = mean_hours_slept))+
  geom_point()+
  ylab("Mean Hours Sleep Per Month")+
  xlab("Migraine Frequency")+
  theme_prism()+
  theme(legend.position = "top")

# statistics

res.aov <- aov(mean_hours_slept ~ attacks_per_month_round, data = sleep_migraine_data)
res.aov

sleep.migraine.lm <- lm(grand_mean_sleep ~ attacks_per_month_round, data = sleep_migraine_data)

summary(sleep.migraine.lm)

# join demographics

full_data <- left_join(demographic_data, sleep_migraine_data, by = "hashed_userid") %>% 
  drop_na(age) %>% 
  filter(attacks_per_month_round <= 29) %>% 
  filter(gender != "Unknown")


# calculate demographics - age
demographic_data <- demographic_data %>% 
  filter(age >= 18 | is.na(age)) %>% 
  drop_na(age)

# gender

gender_data <- demographic_data %>% 
  filter(age >= 18 | is.na(age)) %>% 
  count(gender)

country_data <- demographic_data %>% 
  filter(age >= 18 | is.na(age)) %>% 
  count(country)


# statistics by age

sleep.migraine.lm.gender <- lm(mean_hours_slept ~ age, data = full_data)

summary(sleep.migraine.lm.gender)

# tally demographics

full_data %>% 
 summarise(mean(age))

