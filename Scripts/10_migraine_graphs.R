# overall reports of migraine timing

migraine_events %>% 
  ggplot(aes(x = migraine_start_time_from_midnight))+
  geom_histogram(binwidth = 1, fill = "#ffa600", colour = "black")+
  scale_y_continuous(expand =c(0,0))+
  scale_x_continuous(expand =c(0,0.5), breaks = seq(0,24,by = 4), labels = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00", "00:00"))+
  xlab("Start Time")+
  theme(text = element_text(size = 10))+
  theme_prism()+
  ylab("Number of Migraine Attack Reports")
  
ggsave("Figures/timing_of_attacks_overall.png", dpi = 300, units = "in", width = 5, height = 4)


# migraine duration by region

migraine_region_summary %>% 
  filter(region23 != "NA") %>% 
  mutate(region23 = fct_reorder(region23, attack_duration, .fun='mean')) %>% 
  ggplot(aes(x = region23 , y = attack_duration, fill = attack_duration))+
  geom_col()+
  geom_text(aes(label = round(attack_duration, digits = 2)), hjust = 0, nudge_y = 0.4, size = 3)+ 
  coord_flip() + 
  ylab("Migraine Attack Duration (Hours)")+
  xlab(NULL)+
  theme_prism()+
  expand_limits(y = 35)+
  scale_y_continuous(expand = c(0,0))+
  theme(legend.position = "NULL",
        text = element_text(size = 8))+
  scale_fill_gradientn(colours = c("#FDE0DD", "#FCC5C0", "#FA9FB5", "#F768A1", "#DD3497", "#AE017E", "#7A0177", "#49006A"))

ggsave("Figures/attack_duration_by_region.png", dpi = 300, units = "in", width = 5, height = 4)

# attacks per month according to region

migraine_region_summary %>% 
  filter(region23 != "NA") %>% 
  mutate(region23 = fct_reorder(region23, attacks_per_month, .fun='mean')) %>%
  ggplot(aes(x = region23 , y = attacks_per_month, fill = attacks_per_month))+
  geom_col()+
  geom_text(aes(label = round(attacks_per_month, digits = 2)), hjust = 0, nudge_y = 0.4, size = 3)+ 
  coord_flip() + 
  ylab("Attacks per month")+
  xlab(NULL)+
  theme_prism()+
  scale_y_continuous(expand = c(0,0))+
  expand_limits(y = 7)+
  theme(legend.position = "NULL",
        text = element_text(size = 8))+
  scale_fill_gradientn(colours = c("#FDE0DD", "#FCC5C0", "#FA9FB5", "#F768A1", "#DD3497", "#AE017E"))


ggsave("Figures/attacks_by_region.png", dpi = 300, units = "in", width = 5, height = 4)


# migraine end time

migraine_region_summary  %>% 
  filter(region23 != "NA") %>%
  mutate(region23 = fct_reorder(region23, migraine_end_time_from_midnight, .fun='mean')) %>% 
  ggplot(aes(x = region23 , y = migraine_end_time_from_midnight, fill = as.numeric(region23)))+
  geom_col()+
  geom_text(aes(label = format_time(migraine_end_time_from_midnight)), hjust = 0, nudge_y = 0.4, size = 3) + 
  coord_flip() + 
  ylab("Migraine End Time")+
  xlab(NULL)+
  expand_limits(y = 40)+
  theme_prism()+
  theme(legend.position = "NULL",
        text = element_text(size = 8))+
  scale_y_continuous(expand =c(0,0), limits = c(0, 42), breaks = seq(0,42,by = 8), labels = format_time)+
  scale_fill_gradientn(colours = c("#FDE0DD", "#FCC5C0", "#FA9FB5", "#F768A1", "#DD3497", "#AE017E", "#7A0177", "#49006A"))

ggsave("Figures/migraine_end_time_by_region.png", dpi = 300, units = "in", width = 5, height = 4)

# migraine start time by region


format_time <- function(x) {
  hours <- floor(x) %% 24
  mins <- floor(60 * (x %% 1))
  sprintf("%02d:%02d", hours, mins)
}

migraine_region_summary  %>% 
  filter(region23 != "NA") %>%
  mutate(region23 = fct_reorder(region23, migraine_start_time_from_midnight, .fun='mean')) %>% 
  ggplot(aes(x = region23 , y = migraine_start_time_from_midnight, fill = as.numeric(region23)))+
  geom_col()+
  geom_text(aes(label = format_time(migraine_start_time_from_midnight)), hjust = 0, nudge_y = 0.4, size = 3) + 
  coord_flip() + 
  ylab("Migraine Start Time")+
  xlab(NULL)+
  theme_prism()+
  theme(legend.position = "NULL",
        text = element_text(size = 8))+
  scale_y_continuous(expand =c(0,0), limits = c(0, 18), breaks = seq(0,18,by = 4), labels = format_time)+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "RdPu"))

ggsave("Figures/migraine_start_time_by_region.png", dpi = 300, units = "in", width = 5, height = 4)

