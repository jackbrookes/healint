mean_sd_formatter <- function(x) {
  mean_string <- format(mean(x), nsmall = 2, digits = 2)
  sd_string <- format(sd(x), nsmall = 2, digits = 2)
  paste0(mean_string, " (", sd_string, ")")
}


# demographics

users %>% 
  summarise(population = n()) %>% 
  write_csv("Output/user_summary.csv")

users %>% 
  group_by(gender) %>% 
  summarise(population = n()) %>% 
  write_csv("Output/user_summary_gender.csv")

users %>% 
  group_by(age_category) %>% 
  summarise(population = n()) %>% 
  write_csv("Output/user_summary_age_category.csv")


# sleep

sleep_user_summary %>% 
  summarise(across(c(total_hours_slept:num_interruptions), mean_sd_formatter)) %>%
  pivot_longer(everything()) %>% 
  write_csv("Output/sleep_summary.csv")

sleep_user_summary %>% 
  group_by(gender) %>% 
  summarise(across(c(total_hours_slept:num_interruptions), mean_sd_formatter)) %>%
  pivot_longer(-gender) %>% 
  pivot_wider(names_from = gender, id_cols = name, values_from = value) %>% 
  write_csv("Output/sleep_summary_gender.csv")

sleep_user_summary %>% 
  group_by(age_category) %>% 
  summarise(across(c(total_hours_slept:num_interruptions), mean_sd_formatter)) %>%
  pivot_longer(-age_category) %>% 
  pivot_wider(names_from = age_category, id_cols = name, values_from = value) %>% 
  write_csv("Output/sleep_summary_age.csv")


# migraine

migraine_user_summary %>% 
  summarise(across(c(painintensity:attacks_per_month), mean_sd_formatter)) %>%
  pivot_longer(everything()) %>% 
  write_csv("Output/migraine_summary.csv")

migraine_user_summary %>% 
  group_by(gender) %>% 
  summarise(across(c(painintensity:attacks_per_month), mean_sd_formatter)) %>%
  pivot_longer(-gender) %>% 
  pivot_wider(names_from = gender, id_cols = name, values_from = value) %>% 
  write_csv("Output/migraine_summary_gender.csv")

migraine_user_summary %>% 
  group_by(age_category) %>% 
  summarise(across(c(painintensity:attacks_per_month), mean_sd_formatter)) %>%
  pivot_longer(-age_category) %>% 
  pivot_wider(names_from = age_category, id_cols = name, values_from = value) %>% 
  write_csv("Output/migraine_summary_age.csv")

# migraine frequency

migraine_user_summary %>% 
  group_by(gender, frequency) %>% 
  summarise(num = n(), .groups = "drop_last") %>%
  mutate(portion = num / sum(num), percentage = scales::percent(portion)) %>% 
  write_csv("Output/migraine_frequency_summary_gender.csv")

migraine_user_summary %>% 
  group_by(frequency) %>% 
  summarise(num = n(), .groups = "drop_last") %>%
  mutate(portion = num / sum(num), percentage = scales::percent(portion)) %>% 
  write_csv("Output/migraine_frequency_summary_overall.csv")

