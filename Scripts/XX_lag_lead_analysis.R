prediction_model_data <- combined_daynight_summary %>%
  filter(!missing, !lag(missing, default = TRUE)) %>% 
  inner_join(migraineurs, by = "hashed_userid") %>% 
  mutate(across(c(total_hours_slept, hours_slept_deviation), zscore)) %>% 
  group_by(hashed_userid) %>% 
  mutate(hours_slept_deviation_yesterday = lag(hours_slept_deviation),
         total_hours_slept_yesterday = lag(total_hours_slept),
         num_interruptions_yesterday = lag(total_hours_slept),
         had_attack = attack_duration > 0) %>% 
  filter(!is.na(total_hours_slept_yesterday))

mod_1 <- glmer(
  had_attack ~ total_hours_slept_yesterday +
    hours_slept_deviation_yesterday +
    (1 + hours_slept_deviation_yesterday + total_hours_slept_yesterday | hashed_userid),
  data = prediction_model_data,
  family = "binomial",
  control = glmerControl(optimizer ="Nelder_Mead")
)

mod_2 <- lmer(
  total_pain ~ total_hours_slept_yesterday + (1 + total_hours_slept_yesterday|hashed_userid),
  data = prediction_model_data
)

summary(mod_1)

summary(mod_2)


mod_2 <- lme(
  attack_duration ~ total_hours_slept_yesterday,
  random = ~1|hashed_userid,
  data = prediction_model_data
)


mod_1 %>% 
  ranef() %>% 
  as_tibble() %>% 
  filter(term == "total_hours_slept_yesterday") %>% 
  ggplot(aes(x = condval)) + 
  geom_density()
