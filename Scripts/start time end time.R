ggplot(region_data %>% filter(country == "GB"), aes(x = hour(sleep_starttime_local), color = country)) + 
  geom_density(bw = 0.5)

ggplot(region_data, aes(x = hour(sleep_endtime_local), color = country)) + 
  geom_density(bw = 1)

region_data <- left_join(sleep_data, demographic_data, by = "hashed_userid")
