# overall all reports of migraine timing

migraine_event %>% 
  ggplot(aes(x = migraine_start_time_from_midnight))+
  geom_histogram(binwidth = 1, fill = "purple", colour = "black")+
  theme_prism()+
  scale_y_continuous(expand =c(0,0))+
  scale_x_continuous(expand =c(0,0.5), breaks = seq(0,24,by = 4), labels = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00", "00:00"))+
  xlab("Migraine Start Time")+
  ylab(NULL)
  
ggsave("Figures/timing_of_attacks_overall.png", dpi = 300, units = "in", width = 6.5, height = 5)


# average of average attacks
migraine_user_summary %>% 
  ggplot(aes(x = migraine_start_time_from_midnight))+
  geom_histogram(binwidth = 1, fill = "blue", colour = "black")+
  theme_prism()+
  scale_y_continuous(expand =c(0,0))+
  scale_x_continuous(expand =c(0,0.5), breaks = seq(0,24,by = 4), labels = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00", "00:00"))+
  xlab("Migraine Start Time")+
  ylab(NULL)

ggsave("Figures/timing_of_attacks_average.png", dpi = 300, units = "in", width = 6.5, height = 5)


# migraine start time by region

migraine_region_summary %>% 
  filter(region23 != "NA") %>% 
  ggplot(aes(x = region23 , y = attack_duration, fill = region23))+
  geom_col()+
  coord_flip() + 
  ylab("Migraine Attack Duration (hours)")+
  xlab("Region")+
  theme_prism()+
  scale_y_continuous(expand = c(0,0))+
  theme(legend.position = "NULL")

ggsave("Figures/attack_duration_by_region.png", dpi = 300, units = "in", width = 8, height = 5)


migraine_region_summary %>% 
  filter(region23 != "NA") %>% 
  ggplot(aes(x = region23 , y = attacks_per_month, fill = region23))+
  geom_col()+
  coord_flip() + 
  ylab("Attacks per month")+
  xlab("Region")+
  theme_prism()+
  scale_y_continuous(expand = c(0,0))+
  theme(legend.position = "NULL")

ggsave("Figures/attacks_by_region.png", dpi = 300, units = "in", width = 8, height = 5)

migraine_region_summary %>% 
  filter(region23 != "NA") %>% 
  ggplot(aes(x = region23 , y = migraine_end_time_from_midnight, fill = region23))+
  geom_col()+
  coord_flip() + 
  ylab("Migraine End Time from Midnight (hours)")+
  xlab("Region")+
  theme_prism()+
  scale_y_continuous(expand = c(0,0))+
  theme(legend.position = "NULL")

ggsave("Figures/migraine_end_by_region.png", dpi = 300, units = "in", width = 8, height = 5)

migraine_region_summary %>% 
  filter(region23 != "NA") %>% 
  ggplot(aes(x = region23 , y = migraine_start_time_from_midnight, fill = region23))+
  geom_col()+
  coord_flip() + 
  ylab("Migraine Start Time from Midnight (hours)")+
  xlab("Region")+
  theme_prism()+
  scale_y_continuous(expand = c(0,0))+
  theme(legend.position = "NULL")

ggsave("Figures/migraine_start_by_region.png", dpi = 300, units = "in", width = 8, height = 5)

