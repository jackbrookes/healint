# number of hours after midnight where up until which we assign sleep events as
# belonging to the previous "night"

HOURS_AFTER_MIDNIGHT <- 8

# maximum duration of an attack
MAX_HOURS_ATTACK <- 96 # 4 days

# maximum number attacks for valid data
MAX_NUMBER_ATTACKS <- 25

# number of migraines per month to be included in more sophisticated analysis
MIN_MONTHLY_MIGRAINE_REPORTS <- 8

# number of months in experiment
EXPERIMENT_NUM_MONTHS <- 6

# define a palette of yellow red hues
MAP_COLOUR_PALETTE <- c(
  "#ffdc8a",
  "#ffc885",
  "#ffb584",
  "#ff8e71",
  "#e38d84",
  "#d27e86",
  "#bd7187"
)

# vector of days the experiment spans
EXPERIMENT_DAYS <- seq(ymd("2021-06-30"), ymd("2021-12-31"), by = "1 day")

save_rds_named <- function(x) {
  obj_name <- deparse(substitute(x))
  filepath <- file.path("Processed Data", paste0(obj_name, ".rds"))
  saveRDS(x, filepath)
}

read_rds_named <- function(filename) {
  filepath <- file.path("Processed Data", paste0(filename, ".rds"))
  readRDS(filepath)
}

zscore <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

mean_square_diff <- function(x, y) {
  mean((x - y)^2)
}

calc_metrics <- function(x) {
  hdi_95_region <- hdi(x)
  return(tibble(
    hdi_95_min = hdi_95_region[1],
    hdi_95_max = hdi_95_region[2],
    mean = mean(x)
  ))
}
