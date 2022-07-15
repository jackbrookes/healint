users <- users_raw %>% 
  mutate(country = ifelse(is.na(country), "NA", country),
         region23 = countrycode(country, origin = "iso2c", destination = "region23"),
         age_category = ifelse(age < 35, "Younger adult", "Older adult"))

# tally number of users for each country.
# Note that where there are no users for a country there will be no row.

country_summary <- left_join(users, country_codes_iso, by = c("country" = "code_2")) %>% 
  group_by(region, country) %>% 
  tally(name = "population")

population_summary <- users %>% 
  group_by(age, gender)%>%
  tally() %>% 
  rename(population = n) %>% 
  mutate(population = ifelse(gender == "M", -1*population, population)) %>% 
  na.omit()

