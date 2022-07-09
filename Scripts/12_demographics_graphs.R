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
    colours = MAP_COLOUR_PALETTE,
    trans = "log10",
    limits = c(1, 10000)
  )

ggsave("Figures/world_map.png", width = 8, height = 4)

# plot population pyramid with age and gender


ggplot(population_summary, aes(x = age, fill = gender, y = population))+
  geom_col(data = filter(population_summary, gender == "F")) +
  geom_col(data = filter(population_summary, gender == "M"))+
  geom_col(data = filter(population_summary, gender == "Unknown"))+
  scale_fill_manual(labels = c("Female", "Male", "Unknown"), values = c("#bf6371","#f27853", "#ffa600"))+
  theme_prism()+
  xlab("Age (years)")+
  ylab("Count")+
  scale_y_continuous(limits = c(-120, 120), breaks = c(-120, -80, -40, 0, 40, 80, 120), labels = c(120, 80, 40, 0, 40, 80, 120))+ 
  coord_flip()+
  expand_limits(x = c(18,87))+
  theme(legend.position = "top")

ggsave("Figures/population_pyramid.png", width = 4, height = 4)
