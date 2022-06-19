library(tidyverse)
library(lubridate)
library(ggprism)
library(ggpubr)
library(Hmisc)
library(countrycode)

# required
source("Scripts/01_constants.R")
source("Scripts/02_import.R")

source("Scripts/03_demographics_processing.R")

# run the processing
source("Scripts/04_sleep_processing.R")

# or just read in the processed data
source("Scripts/05_read_sleep.R")

source("Scripts/06_migraine_processing.R")
