# data for each user
users_raw <- read_csv("Data/users.csv") %>% 
  filter(is.na(age) || age >= 18)

# data for the coordinates of different countries with included ISO codes
country_codes_iso <- readRDS("Resources/country_ISO_codes2.rds") %>% 
  rename(region = s_name)

# ISO2 codes which contain codes for each country
world_map_iso <- readRDS("Resources/world_map2.rds") %>% 
  rename(region = country)

sleep_raw <- read_csv("Data/confirmed_sleeps.csv") %>% 
  na.omit()

migraine_raw <- read_csv("Data/migraines_base.csv") %>% 
  na.omit()

