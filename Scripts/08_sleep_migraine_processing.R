combined_user_summary <-
  left_join(
    migraine_user_summary,
    sleep_user_summary,
    by = c(
      "hashed_userid",
      "region23",
      "country",
      "age",
      "gender",
      "age_category"
    )
  )

combined_daynight_summary <- inner_join(
  migraine_day_summary, sleep_night_summary,
  by = c("hashed_userid",
         "region23",
         "country",
         "age",
         "gender",
         "age_category",
         "assigned_day" = "assigned_night")
) %>% 
  rename(assigned_daynight = assigned_day) %>% 
  arrange(hashed_userid, assigned_daynight)
