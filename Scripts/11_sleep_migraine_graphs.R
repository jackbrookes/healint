combined_user_summary %>% 
  ggplot(aes(x = frequency, y = total_hours_slept, fill = frequency))+
  geom_violin(trim = TRUE)+
  ylab("Mean Hours Sleep Per Night")+
  xlab("Migraine Frequency")+
  theme_prism()+
  theme(legend.position = "None",
        text = element_text(size = 10))+
  scale_fill_manual(values = c("#f27993" ,"#ff7d73", "#ff8d49", "#ffa600"))+
  geom_signif(comparisons = list(c("LFEM", "VLFEM")),
                                 map_signif_level= TRUE, 
                                 test = "t.test", 
                                 y_position = c(13), 
                                 tip_length = 0, 
                                 textsize = 3, 
                                 colour = "black")

ggsave("Figures/hours_slept_by_frequency.png", width = 5, height = 4)


# no of interruptions by frequency 

combined_user_summary %>% 
  ggplot(aes(x = frequency, y = num_interruptions, fill = frequency))+
  geom_violin(trim = FALSE)+
  ylab("Sleep Interruptions")+
  xlab("Migraine Frequency")+
  theme_prism()+
  theme(legend.position = "None",
        text = element_text(size = 10))+
  expand_limits(y = c(0,8.5))+
  scale_fill_manual(values = c("#f27993" ,"#ff7d73", "#ff8d49", "#ffa600"))+
  geom_signif(comparisons = list(c("CM", "LFEM"), c("CM", "VLFEM"), c("HFEM", "LFEM"), c("HFEM", "VLFEM")),
              map_signif_level= TRUE, 
              test = "t.test", 
              y_position = c(6,8,5,7), 
              tip_length = 0, 
              textsize = 3, 
              colour = "black")

ggsave("Figures/interruptions_by_frequency.png", width = 5, height = 4)

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

#  cycling effect

ggplot(combined_daynight_pooled_summary, aes(x = days_since_first_attack, y = attack_rate)) + 
  geom_line(colour = "red") +
  theme_prism()+
  facet_wrap(vars(frequency))+
  xlab("Days Since First Attack")+
  ylab("Attack Rate (%)")+
  coord_cartesian(expand = FALSE, xlim = c(4, 150), ylim = c(0, 0.5))

ggsave("Figures/migraine_cycle.png", width = 6, height = 4)
  
