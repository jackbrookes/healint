# data for each user
users_raw <- read_csv("Data/users.csv") %>% 
  filter(is.na(age) | age >= 18)

# data for the coordinates of different countries with included ISO codes
country_codes_iso <- readRDS("Resources/country_ISO_codes2.rds") %>% 
  rename(region = s_name)

# ISO2 codes which contain codes for each country
world_map_iso <- readRDS("Resources/world_map2.rds") %>% 
  rename(region = country)

sleep_raw <- read_csv("Data/confirmed_sleeps.csv") %>% 
  na.omit() %>% 
  distinct() # for some reason there are duplicates

# classification of symptoms into categories

symptoms_classified <- read_csv("Data/symptom_classification.csv")

symptoms_raw <- read_csv("Data/migraine_symptoms.csv") %>% 
  filter(is_default) %>% 
  left_join(symptoms_classified, by = "symptom") %>% 
  na.omit()

migraine_raw <- read_csv("Data/migraines_base.csv") %>% 
  na.omit() %>% 
  distinct() # for some reason there are duplicates
