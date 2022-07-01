# WIP code

# this uses a method similar to the one described here:
# https://stats.stackexchange.com/questions/31666/how-can-i-align-synchronize-two-signals

# basically, we test various offsets (-3, -2, -1, 0, 1..., etc) of days between
# the sleep and migraine data and see which one produces the strongest correlation


laglead_mean_square <- function(offset, col_1, col_2, trim, name) {
  z1 <- zscore(col_1)
  z2 <- zscore(col_2)
  
  len <- length(z1)
  start <- 1 + floor(trim / 2)
  end <- len - ceil(trim / 2)
  
  slice1 <- (start:end) - floor(offset / 2)
  slice2 <- (start:end) + ceil(offset / 2)
  
  cor(z1[slice1], z2[slice2], use = "complete.obs")
}

align_datasets <- function(col_1, col_2, test_offsets, name) {
  mean_squares <- map_dbl(test_offsets,
                          laglead_mean_square,
                          col_1 = col_1,
                          col_2 = col_2,
                          name = name,
                          trim = max(abs(test_offsets)))
  tibble(min_mean_squares = max(mean_squares),
         best_alignment = test_offsets[which.max(mean_squares)])
}


aligned_daynight_summary <- combined_daynight_summary %>%
  inner_join(migraineurs, by = "hashed_userid") %>% 
  group_by(hashed_userid,
           region23,
           country,
           age,
           gender,
           age_category) %>% 
  filter(any(total_pain > 0)) %>% 
  summarise(align_datasets(-attack_duration,
                           total_hours_slept,
                           -15:15,
                           hashed_userid),
            .groups = "drop")

aligned_daynight_summary %>% 
  ggplot(aes(x = best_alignment)) + 
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 0)

aligned_daynight_summary %>% 
  ggplot(aes(x = best_alignment)) + 
  geom_density(color="black", fill="lightblue")+ 
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_prism()+
  xlab("Best Alignment")+
  ylab(NULL)+
  coord_cartesian(expand = c(0,0))+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10))+
  scale_y_continuous(limits = c(0,0.05))

ggsave("Figures/model.png", dpi = 300, units = ("in"), width = 3, height = 3)

t.test(aligned_daynight_summary$best_alignment)

combined_daynight_summary %>%
  inner_join(migraineurs, by = "hashed_userid") %>% 
  lmtest::grangertest(attack_duration ~ total_hours_slept | hashed_userid, data = .)

# viz stuff

one_participant <- combined_daynight_summary %>% 
  filter(hashed_userid == "1e3a356d3c7fae8b0df1be9e5454a6ae94ecd943b75f71608e053c73eb5d967e")

with(one_participant, laglead_mean_square(-30, -attack_duration, total_hours_slept, 30))