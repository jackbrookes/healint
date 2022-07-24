migraine_normalisation <- combined_daynight_summary %>%
  group_by(hashed_userid) %>% 
  filter(!lag(missing, default = TRUE)) %>% 
  ungroup() %>% 
  inner_join(migraineurs, by = "hashed_userid") %>% 
  summarise(across(total_hours_slept, list(mean = mean, sd = sd)))
  

migraine_prediction_data <- combined_daynight_summary %>%
  filter(!lag(missing, default = TRUE)) %>% 
  inner_join(migraineurs, by = "hashed_userid") %>% 
  mutate(total_hours_slept = (total_hours_slept - migraine_normalisation$total_hours_slept_mean) / migraine_normalisation$total_hours_slept_sd) %>% # recenter, helps with convergence
  group_by(hashed_userid) %>% 
  mutate(hours_slept_zdeviation_yesterday = lag(hours_slept_zdeviation),
         total_hours_slept_yesterday = lag(total_hours_slept),
         num_interruptions_yesterday = lag(num_interruptions),
         had_attack = as.numeric(attack_duration > 0)) %>% 
  ungroup() %>% 
  filter(!is.na(total_hours_slept_yesterday)) %>% 
  select(hashed_userid, had_attack, num_interruptions_yesterday, total_hours_slept_yesterday, hours_slept_zdeviation_yesterday)
# %>% filter(hashed_userid %in% sample(unique(hashed_userid), 25))

# brm

migraine_bayes <- brm(
  had_attack ~ num_interruptions_yesterday +
    total_hours_slept_yesterday + 
    hours_slept_zdeviation_yesterday + 
    (1 + num_interruptions_yesterday + total_hours_slept_yesterday + hours_slept_zdeviation_yesterday | hashed_userid),
  data = migraine_prediction_data,
  family = "bernoulli",
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 1000,
  prior = prior(normal(0,100), class = b) +
    prior(normal(0,100), class = sd, group = hashed_userid),
  control = list(adapt_delta = .95)
)

plot(migraine_bayes, variable = "^b_", regex = TRUE)

# save

save_rds_named(migraine_bayes)
