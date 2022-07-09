combined_user_summary %>% 
  ggplot(aes(x = frequency, y = total_hours_slept, fill = frequency))+
  geom_violin(trim = TRUE)+
  ylab("Mean Hours Sleep Per Night")+
  xlab("Migraine Frequency")+
  theme_prism()+
  theme(legend.position = "top")

ggsave("Figures/hours_slept_by_frequency.png", width = 6, height = 5)

combined_user_summary %>% 
  filter(gender != "NA" & gender != "Unknown") %>% 
  ggplot(aes(x = attacks, y = num_interruptions))+
  geom_point(alpha = 0.3, colour = "purple")+
  geom_smooth(method = "lm", colour = "black", alpha = 0.2) +
  ylab("Mean Nightly Sleep Interruptions")+
  xlab("Number of Attacks")+
  theme_prism()+
  coord_cartesian(ylim = c(0,1))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  theme(legend.position = "top")


ggsave("Figures/number_of_interruptions_by_attacks.png", width = 6, height = 5)

combined_user_summary %>% 
  ggplot(aes(x = attacks_per_month, y = sleep_efficiency))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylab("Sleep Efficiency (%)")+
  xlab("Number of Attacks")+
  theme_prism()+
  theme(legend.position = "top")

ggsave("Figures/sleep_efficiency_by_attacks.png", width = 6, height = 5)

combined_user_summary %>% 
  ggplot(aes(x = attacks_per_month, y = total_hours_slept))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylab("Hours Slept")+
  xlab("Number of Attacks")+
  theme_prism()+
  theme(legend.position = "top")

ggsave("Figures/hours_slept_by_attacks.png", width = 6, height = 5)

combined_user_summary %>% 
  ggplot(aes(x = attacks_per_month, y = wake_time))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylab("Time Awake (hours)")+
  xlab("Number of Attacks")+
  theme_prism()+
  theme(legend.position = "top")

ggsave("Figures/time_awake_by_attacks.png", width = 6, height = 5)

combined_user_summary %>% 
  ggplot(aes(x = painintensity, y = total_hours_slept))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylab("Hours Slept")+
  xlab("Pain Intensity")+
  theme_prism()+
  theme(legend.position = "top")

ggsave("Figures/hours_slept_by_intensity.png", width = 6, height = 5)


