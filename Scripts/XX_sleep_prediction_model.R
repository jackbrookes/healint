sleep_prediction_data <- combined_daynight_summary %>%
  filter(!lead(missing, default = TRUE)) %>% 
  inner_join(migraineurs, by = "hashed_userid") %>% 
  mutate(had_attack = attack_duration > 0, 
         across(c(painintensity, total_pain, attack_duration), zscore)) # recenter these columns, helps with convergence

# model for each predictor

sleep_mod_hadattack <- lmer(
  total_hours_slept ~ had_attack +
    (1 + had_attack | hashed_userid),
  data = sleep_prediction_data,
  control = lmer_control_list
)

summary(sleep_mod_hadattack)


sleep_mod_pain <- lmer(
  total_hours_slept ~ painintensity +
    (1 + painintensity | hashed_userid),
  data = sleep_prediction_data,
  control = lmer_control_list
)

summary(sleep_mod_pain)

sleep_mod_duration <- lmer(
  total_hours_slept ~ attack_duration +
    (1 + attack_duration | hashed_userid),
  data = sleep_prediction_data,
  control = lmer_control_list
)

summary(sleep_mod_duration)

# combine

sleep_mod_all <- lmer(
  total_hours_slept ~ attack_duration *
    painintensity + 
    (1 + attack_duration * painintensity | hashed_userid),
  data = sleep_prediction_data,
  control = lmer_control_list
)

summary(sleep_mod_all)

sleep_mod_all <- brm(
  total_hours_slept ~ had_attack *
    painintensity + 
    (1 + had_attack * painintensity | hashed_userid),
  data = sleep_prediction_data,
  warmup = 1000,
  iter = 2000, chains = 4, control = list(adapt_delta = 0.95)
)

summary(sleep_mod_all)


# save

save_rds_named(sleep_mod_interruptions)
save_rds_named(sleep_mod_hours)
save_rds_named(sleep_mod_deviation)
save_rds_named(sleep_mod_all)

sleep_fixed_effects <- sleep_mod_all %>%
  tidy() %>% 
  filter(effect == "fixed")

sleep_mod_all %>% 
  ranef() %>% 
  as_tibble() %>% 
  left_join(sleep_fixed_effects, by = "term") %>% 
  filter(term != "(Intercept)") %>% 
  ggplot() + 
  geom_density(aes(x = estimate + condval, fill = term)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(term), scales = "free") +
  labs(x = "Beta value estimate", y = "Density across population") +
  theme_prism() +
  theme(legend.position = "none")

ggsave("Figures/sleep_mod_estimates.png", width = 9, height = 3)
