sleep_region_summary %>% 
  filter(region23 != "NA") %>% 
  ggplot(aes(x = region23 , y = total_hours_slept, fill = region23))+
  geom_col()+
  coord_flip() + 
  ylab("Mean Hours Slept")+
  xlab("Region")+
  theme_prism()+
  scale_y_continuous(expand = c(0,0))+
  theme(legend.position = "NULL")

ggsave("Figures/hours_slept_by_region.png", dpi = 300, units = "in", width = 8, height = 5)


sleep_region_summary %>% 
  filter(region23 != "NA") %>% 
  ggplot(aes(x = region23 , y = start_time_from_midnight, fill = region23))+
  geom_col()+
  coord_flip() + 
  ylab("Sleep Start Time")+
  xlab("Region")+
  theme_prism()+
  scale_y_continuous(expand = c(0,0))+
  theme(legend.position = "NULL")

ggsave("Figures/start_time_by_region.png", dpi = 300, units = "in", width = 8, height = 5)



