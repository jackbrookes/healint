library(Hmisc)
library(tidyverse)
library(lubridate)
library(ggprism)
library(ggpubr)
library(countrycode)
library(brms)
library(HDInterval)
library(ggsignif)
library(foreign)
library(MASS)
library(ggplot2)
library(reshape2)
library(dplyr)

# required
source("Scripts/01_constants.R")
source("Scripts/02_import.R")
source("Scripts/03_demographics_processing.R")

# run the processing
source("Scripts/04_sleep_processing.R")
source("Scripts/05_migraine_processing.R")

# or just read in the processed data
source("Scripts/06_read_sleep.R")
source("Scripts/07_read_migraine.R")

# combine the data
source("Scripts/08_sleep_migraine_processing.R")

# graphs
source("Scripts/09_sleep_graphs.R")
source("Scripts/10_migraine_graphs.R")
source("Scripts/11_sleep_migraine_graphs.R")
source("Scripts/12_demographics_graphs.R")

# output
source("Scripts/13_summary_tables.R")
source("Scripts/14_statistics.R")
