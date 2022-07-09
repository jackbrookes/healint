migraine_prediction_data <- combined_daynight_summary %>%
  filter(!lag(missing, default = TRUE)) %>% 
  inner_join(migraineurs, by = "hashed_userid") %>% 
  mutate(across(c(total_hours_slept), zscore)) %>% # recenter these columns, helps with convergence
  group_by(hashed_userid) %>% 
  mutate(hours_slept_zdeviation_yesterday = lag(hours_slept_zdeviation),
         total_hours_slept_yesterday = lag(total_hours_slept),
         num_interruptions_yesterday = lag(num_interruptions),
         had_attack = attack_duration > 0) %>% 
  filter(!is.na(total_hours_slept_yesterday))

# model for each predictor

migraine_mod_interruptions <- glmer(
  had_attack ~ num_interruptions_yesterday +
    (1 + num_interruptions_yesterday | hashed_userid),
  data = migraine_prediction_data,
  family = "binomial",
  control = glmer_control_list
)

summary(migraine_mod_interruptions)


migraine_mod_hours <- glmer(
  had_attack ~ total_hours_slept_yesterday +
    (1 + total_hours_slept_yesterday | hashed_userid),
  data = migraine_prediction_data,
  family = "binomial",
  control = glmer_control_list
)

summary(migraine_mod_hours)

migraine_mod_deviation <- glmer(
  had_attack ~ hours_slept_zdeviation_yesterday +
    (1 + hours_slept_zdeviation_yesterday | hashed_userid),
  data = migraine_prediction_data,
  family = "binomial",
  control = glmer_control_list
)

summary(migraine_mod_deviation)

# combine

migraine_mod_all <- glmer(
  had_attack ~ num_interruptions_yesterday +
    total_hours_slept_yesterday + 
    hours_slept_zdeviation_yesterday + 
    (1 + num_interruptions_yesterday + total_hours_slept_yesterday + hours_slept_zdeviation_yesterday | hashed_userid),
  data = migraine_prediction_data,
  family = "binomial",
  control = glmer_control_list
)

summary(migraine_mod_all)


# save

save_rds_named(migraine_mod_interruptions)
save_rds_named(migraine_mod_hours)
save_rds_named(migraine_mod_deviation)
save_rds_named(migraine_mod_all)

fixed_effects <- migraine_mod_all %>%
  tidy() %>% 
  filter(effect == "fixed")

migraine_mod_all %>% 
  ranef() %>% 
  as_tibble() %>% 
  left_join(fixed_effects, by = "term") %>% 
  ggplot() + 
  geom_density(aes(x = estimate + condval, fill = term)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(term), scales = "free") +
  theme_prism() +
  theme(legend.position = "none")
