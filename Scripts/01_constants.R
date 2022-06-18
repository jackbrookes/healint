# number of hours after midnight where up until which we assign sleep events as
# belonging to the previous "night"

HOURS_AFTER_MIDNIGHT <- 8

# maximum duration of an attack

MAX_HOURS_ATTACK <- 96 # 4 days

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

save_rds_named <- function(x) {
  obj_name <- deparse(substitute(x))
  filepath <- file.path("Processed Data", paste0(obj_name, ".rds"))
  saveRDS(x, filepath)
}

read_rds_named <- function(filename) {
  filepath <- file.path("Processed Data", paste0(filename, ".rds"))
  readRDS(filepath)
}

