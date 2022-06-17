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

# Read in the sleep data

sleep_data_raw <- read_csv("Data/confirmed_sleeps.csv") %>% 
  na.omit()

# Extract day and month

demographic_data <- read_csv("Data/users.csv") %>% 
  mutate(region = countrycode(country, origin = "iso2c", destination = "region23")) 

## run for a sample hashed_userid_sample <- demographic_data %>% 
  # sample_n(2000) %>% 
  # pull(hashed_userid)

sleep_data <- sleep_data_raw %>% 
  # filter(hashed_userid %in% hashed_userid_sample) %>% 
  mutate(hours_slept = as.numeric(difftime(sleep_endtime_local,sleep_starttime_local, units="hours")),
         night = floor_date(sleep_starttime_local - hours(8), unit = "days"), # make a note of this
         start_time = as.duration(interval(night, sleep_starttime_local))) %>% 
  left_join(demographic_data, by = "hashed_userid")

# Work out mean hours slept per night

sleep_data_night_summary <- sleep_data %>% 
  group_by(hashed_userid, night, region, country) %>% 
  summarise(mean_hours_slept = mean(hours_slept),
            start_time = min(start_time))

sleep_data_user_summary <- sleep_data_night_summary %>%
  group_by(hashed_userid, region, country) %>% 
  summarise(grand_mean_sleep = mean(mean_hours_slept),
            grand_mean_start_time = mean(start_time))

demographic_data <- read_csv("Data/users.csv") %>% 
  filter(age >= 18)

sleep_user_data <- left_join(sleep_data_user_summary,demographic_data, by = c("hashed_userid", "country"))

# Work out sleep start time per country

sleep_data_region_summary <- sleep_data_user_summary %>% 
  group_by(region) %>% 
  summarise(grand_mean_start_time = seconds(mean(grand_mean_start_time)), 
            sd_start_time = seconds(sd(grand_mean_start_time))) %>% 
  mutate(region = fct_reorder(region, grand_mean_start_time))

saveRDS(sleep_data_night_summary, file = "Data/sleep_start_time_by_region.rds")
sleep_start_region <- readRDS("Data/sleep_start_time_by_region.rds")

# Work out hours slept per country

sleep_hours_region_summary <- sleep_start_region %>% 
  summarise(mean_start_time = seconds(mean(grand_mean_start_time)))


# Plot start time according to region

sleep_data_region_summary %>% 
  ggplot(aes(x = region, y = grand_mean_start_time, fill = region))+
  geom_col() + 
  coord_flip(ylim = c(60*60*16, 60*60*27)) + 
  scale_y_time(breaks = seq(0, 60*60*30, 2*60*60), expand = c(0,0))+
  ylab("Mean Sleep Start Time")+
  xlab("Region")+
  theme_prism()+
  theme(axis.text.x=element_text(angle = -45, hjust = 0),
        legend.position = "NULL")


ggsave("Figures/start_time_by_region.png", dpi = 300, type = "cairo", units = "in", width = 8, height = 5)

# Summarise overall average sleep start time


sleep_data_region_summary %>% 
  summarise(mean = mean(grand_mean_start_time))


# remove outliers from hours slept 

Q <- quantile(sleep_data$grand_mean_sleep, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(sleep_data$grand_mean_sleep)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
eliminated_sleep <- subset(sleep_data, sleep_data$grand_mean_sleep > (Q[1] - 1.5*iqr) & sleep_data$grand_mean_sleep < (Q[2]+1.5*iqr))