combined_user_summary <- left_join(migraine_user_summary, sleep_user_summary, by = c("hashed_userid", "region23", "country", "age", "gender", "age_category"))

                                   