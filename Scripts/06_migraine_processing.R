# dataframe with one row per migraine event
# we can use month here becuse we only have 6 months worth of data

migraine_event <- migraine_raw %>% 
  mutate(assigned_day = floor_date(migraine_starttime_local, unit = "days"),
         month = month(migraine_starttime_local),
         attack_duration = as.numeric(interval(migraine_starttime_local, migraine_endtime_local), "hours"),
         migraine_start_time_from_midnight = as.numeric(interval(assigned_day, migraine_starttime_local), "hours"),
         migraine_end_time_from_midnight = as.numeric(interval(assigned_day, migraine_endtime_local), "hours")
         ) %>%
  filter(attack_duration <= MAX_HOURS_ATTACK) %>% 
  inner_join(users, by = "hashed_userid")

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
  summarise(across(c(painintensity, attack_duration:migraine_end_time_from_midnight), mean),
            attacks = n(),
            trigger_menstruation = sum(trigger_menstruation) / attacks,
            .groups = "drop") %>% 
  left_join(migraine_user_month_summary, by = "hashed_userid")

# Summary of migraine characteristics per region

migraine_region_summary <- migraine_user_summary %>%
  group_by(region23) %>% 
  summarise(across(painintensity:attacks_per_month, mean), .groups = "drop")

