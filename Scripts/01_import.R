# data for each user
users_raw <- read_csv("Data/users.csv") %>% 
  mutate(country = ifelse(is.na(country), "NA", country))

# data for the coordinates of different countries with included ISO codes
country_codes_iso <- readRDS("Resources/country_ISO_codes2.rds") %>% 
  rename(region = s_name)

# ISO2 codes which contain codes for each country
world_map_iso <- readRDS("Resources/world_map2.rds") %>% 
  rename(region = country)

# define a palette of yellow red hues
map_colour_palette <- c(
  "#ffdc8a",
  "#ffc885",
  "#ffb584",
  "#ff8e71",
  "#e38d84",
  "#d27e86",
  "#bd7187"
)

