
# dataframe with one row per migraine event
# we can use month here because we only have 6 months worth of data

migraine_events <- migraine_raw %>% 
  mutate(assigned_day = floor_date(migraine_starttime_local, unit = "days"),
         month = month(migraine_starttime_local),
         attack_duration = as.numeric(interval(migraine_starttime_local, migraine_endtime_local), "hours"),
         total_pain = attack_duration * painintensity,
         migraine_start_time_from_midnight = as.numeric(interval(assigned_day, migraine_starttime_local), "hours"),
         migraine_end_time_from_midnight = as.numeric(interval(assigned_day, migraine_endtime_local), "hours")
         ) %>%
  filter(attack_duration <= MAX_HOURS_ATTACK) %>%
  group_by(hashed_userid) %>% 
  filter(n() <= (MAX_NUMBER_ATTACKS * EXPERIMENT_NUM_MONTHS)) %>%
  ungroup() %>% 
  inner_join(users, by = "hashed_userid")

# these users meet our criteria for more sophisticated analysis
migraineurs <- migraine_events %>% 
  group_by(hashed_userid) %>% 
  count() %>% 
  filter(n >= MIN_MONTHLY_MIGRAINE_REPORTS * EXPERIMENT_NUM_MONTHS) %>% 
  select(hashed_userid)

# function to take a start, end time and split it into pairs of hours.
# e.g. start=20, end=50 would result in:
# start=c(20,24,48), end=c(24,48,50)
split_attack_days <- function(migraine_start_time_from_midnight,
                              migraine_end_time_from_midnight) {
  span_days <- 1 + (migraine_end_time_from_midnight %/% 24)
  day_vec <- 1:span_days
  tibble(migraine_start_time_from_midnight = pmax(24.0 * (day_vec - 1),
                                               rep(migraine_start_time_from_midnight, span_days)),
         migraine_end_time_from_midnight = pmin(rep(migraine_end_time_from_midnight, span_days),
                                              24.0 * day_vec),
         attack_day = day_vec)
}

# Summary of migraine for each calendar day
# Some weirdness here due to change of time zones (?)
# It's possible that there are more that 24h of attacks in a day

migraine_day_summary <- migraine_events %>% 
  group_by(hashed_userid, hashed_migraineid, assigned_day, painintensity, region23, country, age, gender, age_category) %>% 
  summarise(split_attack_days(migraine_start_time_from_midnight, migraine_end_time_from_midnight),
            .groups = "drop") %>%
  mutate(assigned_day = assigned_day + days(attack_day - 1),
         attack_duration = migraine_end_time_from_midnight - migraine_start_time_from_midnight,
         total_pain = attack_duration * painintensity) %>% 
  group_by(hashed_userid, assigned_day, region23, country, age, gender, age_category) %>% 
  summarise(across(c(painintensity, attack_duration, total_pain), sum), .groups = "drop") %>% # collapse to one row per day
  group_by(hashed_userid) %>% 
  complete(nesting(region23, country, age, gender, age_category),
           assigned_day = EXPERIMENT_DAYS, # fill in missing days with 0
           fill = list(attack_duration = 0,
                       painintensity = 0,
                       total_pain = 0)) %>% 
  ungroup()
  

# Summary of migraine monthly data per user

migraine_user_month_summary <- migraine_events %>% 
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

migraine_user_summary <- migraine_events %>% 
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

# save RDS

save_rds_named(migraine_events)
save_rds_named(migraineurs)
save_rds_named(migraine_day_summary)
save_rds_named(migraine_user_month_summary)
save_rds_named(migraine_user_summary)


