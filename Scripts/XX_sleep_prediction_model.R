sleep_normalisation <- combined_daynight_summary %>%
  inner_join(migraineurs, by = "hashed_userid") %>% 
  summarise(across(c(painintensity, total_pain, attack_duration), list(mean = mean, sd = sd)))


sleep_prediction_data <- combined_daynight_summary %>%
  filter(!lead(missing, default = TRUE)) %>% 
  inner_join(migraineurs, by = "hashed_userid") %>% 
  mutate(had_attack = as.numeric(attack_duration > 0), 
         painintensity = (painintensity - sleep_normalisation$painintensity_mean) / sleep_normalisation$painintensity_sd,
         total_pain = (total_pain - sleep_normalisation$total_pain_mean) / sleep_normalisation$total_pain_sd,
         attack_duration = (attack_duration - sleep_normalisation$attack_duration_mean) / sleep_normalisation$attack_duration_sd) %>%  # recenter  helps with convergence
  select(hashed_userid, total_hours_slept, had_attack, painintensity, total_pain)

# bayes

sleep_bayes <- brm(
  total_hours_slept ~ had_attack + painintensity +
    (1 + had_attack + painintensity | hashed_userid),
  data = sleep_prediction_data,
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 1000,
  prior = prior(normal(0,100), class = b) +
    prior(normal(0,100), class = sd, group = hashed_userid),
  control = list(adapt_delta = .95)
)

plot(sleep_bayes, variable = "^b_", regex = TRUE)

# save

save_rds_named(sleep_normalisation)
save_rds_named(sleep_bayes)
