sleep_region_summary %>% 
  filter(region23 != "NA") %>% 
  mutate(region23 = fct_reorder(region23, total_hours_slept, .fun='mean')) %>% 
  ggplot(aes(x = region23 , y = total_hours_slept, fill = as.numeric(region23)))+
  geom_col()+
  geom_text(aes(label = round(total_hours_slept, digits = 2)), hjust = 0, nudge_y = 0.4, size = 3)+ 
  coord_flip() + 
  ylab("Mean Hours Slept")+
  xlab(NULL)+
  theme_prism()+
  expand_limits(y = 10) +
  scale_y_continuous(expand = c(0,0))+
  theme(legend.position = "NULL",
        text = element_text(size = 8))+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "Oranges"))

ggsave("Figures/hours_slept_by_region.png", dpi = 300, units = "in", width = 5, height = 4)

format_time <- function(x) {
  hours <- floor(x) %% 24
  mins <- floor(60 * (x %% 1))
  sprintf("%02d:%02d", hours, mins)
}

sleep_region_summary %>% 
  filter(region23 != "NA") %>%
  mutate(region23 = fct_reorder(region23, start_time_from_midnight, .fun='mean')) %>% 
  ggplot(aes(x = region23 , y = start_time_from_midnight, fill = as.numeric(region23)))+
  geom_col()+
  geom_text(aes(label = format_time(start_time_from_midnight)), hjust = 0, nudge_y = 0.4, size = 3) + 
  coord_flip() + 
  ylab("Sleep Start Time")+
  xlab(NULL)+
  theme_prism()+
  theme(legend.position = "NULL",
        text = element_text(size = 8))+
  scale_y_continuous(expand =c(0,0), limits = c(0, 29), breaks = seq(0,29,by = 4), labels = format_time)+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "Oranges"))


ggsave("Figures/start_time_by_region.png", dpi = 300, units = "in", width = 5, height = 4)



