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
  arrange(hashed_userid, assigned_daynight) %>% 
  left_join(select(combined_user_summary, frequency, hashed_userid), by = "hashed_userid")

# cycling (split into episodic and chronic migraine according to ICHD-3 critera)

combined_daynight_pooled_summary <- combined_daynight_summary %>% 
  mutate(frequency = case_when(frequency == "VLFEM" ~ "EM",
                   frequency == "LFEM" ~ "EM",
                   frequency == "HFEM" ~ "EM",
                   TRUE ~ "CM"
                   )) %>% 
  group_by(hashed_userid, frequency) %>% 
  mutate(days_since_first_attack = as.numeric(interval(first(assigned_daynight[attack_duration > 0]), assigned_daynight), "days")) %>% 
  ungroup() %>% 
  filter(days_since_first_attack >= 0) %>% 
  group_by(days_since_first_attack, frequency) %>% 
  summarise(attack_rate = mean(attack_duration > 0), .groups = "drop")

# fourier test

combined_fft_dayssince_summary <- combined_daynight_pooled_summary %>% 
  summarise(tibble(freq = seq_along(EXPERIMENT_DAYS), fft_data = fft(attack_rate)), .groups = "drop") %>% 
  mutate(power = abs(Re(fft_data)))
  
ggplot(combined_fft_dayssince_summary, aes(x = freq, y = power)) + 
  geom_line() +
  geom_point() +
  coord_cartesian(xlim = c(0, 15), ylim = c(0, 5))

# per user

combined_fft_summary <- combined_daynight_summary %>%
  mutate(frequency = case_when(frequency == "VLFEM" ~ "EM",
                               frequency == "LFEM" ~ "EM",
                               frequency == "HFEM" ~ "EM",
                               TRUE ~ "CM"
  )) %>% 
  group_by(hashed_userid, frequency) %>% 
  summarise(tibble(freq = seq(0, 1, length.out = length(EXPERIMENT_DAYS)), fft_data = fft(attack_duration > 0)), .groups = "drop") %>% 
  group_by(freq, frequency) %>% 
  summarise(power = mean(abs(Re(fft_data))), .groups = "drop") %>% 
  mutate(period = 1.0 / freq)


ggplot(combined_fft_summary, aes(x = freq, y = power, colour = frequency)) + 
  geom_line()+
  coord_cartesian(xlim = c(0.138, 0.145), ylim = c(0, 7.5))+
  theme_prism()+
  expand_limits(x = c(0,0))+
  theme(legend.position = "top",
        text = element_text(size= 8))+
  xlab("Attack Frequency")+
  ylab("Power")

ggsave("Figures/fft_migraine_attack_occurrence_by_frequency.png", dpi = 300, units = "in", width = 3.5, height = 3)  

