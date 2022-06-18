
demographic_data <- users_raw %>% 
  filter(age >= 18)%>% 
  group_by(age, gender, country)%>%
  tally() %>% 
  rename(population = n)


# tally number of users for each country.
# Note that where there are no users for a country there will be no row.

country_summary <- left_join(users_raw, country_codes_iso, by = c("country" = "code_2")) %>% 
  group_by(region, country) %>% 
  tally(name = "population")


# Plot the map. We first plot all countries in a solid color, the countries with 
# users, then an outline of all countries on top.

country_codes_iso %>% 
  filter(region != "Antarctica") %>% 
  ggplot(aes(map_id = region)) +
  geom_map(map = world_map_iso, fill = "#e0e0e0") + # blank countries
  geom_map(aes(fill = population), map = world_map_iso, data = country_summary) + # filled countries
  geom_map(map = world_map_iso, color = "#101010", size = 0.1, alpha = 0.0) + # outline
  expand_limits(x = c(-180, 180), y = c(-55, 85)) + 
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(0.4, "in"),
    plot.background = element_rect(fill = "white", size = 0)
  ) +
  expand_limits(fill = 0) +
  coord_fixed() +
  labs(fill = "Number of users") +
  scale_fill_gradientn(
    colours = map_colour_palette,
    trans = "log10",
    limits = c(1, 10000)
  )

ggsave("Figures/world_map.png", width = 8, height = 4)
