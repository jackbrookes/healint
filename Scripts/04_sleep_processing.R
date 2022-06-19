# dataframe with one row per sleep event

sleep_events <- sleep_raw %>% 
  mutate(hours_slept = as.numeric(interval(sleep_starttime_local, sleep_endtime_local), "hours"),
         assigned_night = floor_date(sleep_starttime_local - hours(HOURS_AFTER_MIDNIGHT), unit = "days"),
         start_time_from_midnight = as.numeric(interval(assigned_night, sleep_starttime_local), "hours"),
         end_time_from_midnight = as.numeric(interval(assigned_night, sleep_endtime_local), "hours")) %>% 
  inner_join(users, by = "hashed_userid")

# Summary of sleep characteristics per night per user

sleep_night_summary <- sleep_events %>% 
  group_by(hashed_userid, assigned_night, region23, country, age, gender, age_category) %>% 
  summarise(total_hours_slept = sum(hours_slept),
            start_time_from_midnight = min(start_time_from_midnight),
            end_time_from_midnight = max(end_time_from_midnight),
            time_in_bed = end_time_from_midnight - start_time_from_midnight,
            wake_time = time_in_bed - total_hours_slept,
            sleep_efficiency = total_hours_slept / time_in_bed,
            num_interruptions = n() - 1,
            .groups = "drop")

# Summary of sleep characteristics per user

sleep_user_summary <- sleep_night_summary %>%
  group_by(hashed_userid, region23, country, age, gender, age_category) %>% 
  summarise(across(total_hours_slept:num_interruptions, mean), .groups = "drop")

# Summary of sleep characteristics per region

sleep_region_summary <- sleep_user_summary %>%
  group_by(region23) %>% 
  summarise(across(total_hours_slept:num_interruptions, mean), .groups = "drop")

# save RDS

save_rds_named(sleep_events)
save_rds_named(sleep_night_summary)
save_rds_named(sleep_user_summary)
save_rds_named(sleep_region_summary)
