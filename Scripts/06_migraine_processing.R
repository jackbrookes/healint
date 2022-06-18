# dataframe with one row per migraine event
# we can use month here becuse we only have 6 months worth of data

migraine_event <- migraine_raw %>% 
  mutate(attack_duration = as.numeric(interval(migraine_starttime_local, migraine_endtime_local), "hours"),
         month = month(migraine_starttime_local)) %>%
  filter(attack_duration <= MAX_HOURS_ATTACK) %>% 
  left_join(users, by = "hashed_userid")

# Summary of migraine monthly data per user

migraine_user_month_summary <- migraine_event %>% 
  group_by(hashed_userid, month) %>% 
  summarise(attacks = n(), .groups = "drop") %>% 
  complete(hashed_userid, month, fill = list(attacks = 0)) %>%
  group_by(hashed_userid) %>% 
  summarise(attacks_per_month = mean(attacks), .groups = "drop") %>% 
  mutate(frequency = case_when(attacks_per_month <= 3.5 ~ "VLFEM",
                               3.5 < attacks_per_month & attacks_per_month <= 7.5 ~ "LFEM",
                               7.5 < attacks_per_month & attacks_per_month <= 14.5 ~ "HFEM",
                               14.5 < attacks_per_month ~ "CM"))
  
# Summary of migraine per user

migraine_user_summary <- migraine_event %>% 
  group_by(hashed_userid, region23, country, age, gender, age_category) %>% 
  summarise(across(c(painintensity, attack_duration), mean),
            attacks = n(),
            trigger_menstruation = sum(trigger_menstruation) / attacks,
            .groups = "drop") %>% 
  left_join(migraine_user_month_summary, by = "hashed_userid")
