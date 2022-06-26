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
source("Scripts/07_sleep_migraine_processing.R")

# graphs
source("Scripts/08_sleep_graphs.R")
source("Scripts/09_migraine_graphs.R")
source("Scripts/10_sleep_migraine_graphs.R")
source("Scripts/11_demographics_graphs.R")

# output
source("Scripts/12_summary_tables.R")
source("Scripts/13_statistics.R")
