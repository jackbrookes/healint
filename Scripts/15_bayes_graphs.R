migraine_bayes <- read_rds_named("migraine_bayes")
sleep_bayes <- read_rds_named("sleep_bayes")
migraine_normalisation <- read_rds_named("migraine_normalisation")
sleep_normalisation <- read_rds_named("sleep_normalisation")

# process the data

migraine_posterior <- as_draws_df(migraine_bayes, "^b_", regex = TRUE) %>% 
  as_tibble() %>%
  rename_with(~ str_remove(., "b_")) %>% 
  select(-starts_with("."), -"Intercept") %>% 
  # undo normalization
  mutate(total_hours_slept_yesterday =
           (total_hours_slept_yesterday * migraine_normalisation$total_hours_slept_sd)) %>% 
  rename(`Num Interruptions Yesterday` = num_interruptions_yesterday,
         `Total Hours Slept Yesterday` = total_hours_slept_yesterday,
         `Hours Slept Yesterday (z)` = hours_slept_zdeviation_yesterday) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "sample")

sleep_posterior <- as_draws_df(sleep_bayes, "^b_", regex = TRUE) %>% 
  as_tibble() %>%
  rename_with(~ str_remove(., "b_")) %>% 
  select(-starts_with("."), -"Intercept") %>% 
  # undo normalization
  mutate(painintensity =
           (painintensity * sleep_normalisation$painintensity_sd)) %>% 
  rename(`Had Attack` = had_attack,
         `Pain Intensity` = painintensity) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "sample")


# calculate Highest Density Interval, mean sample, etc

migraine_hdi <- migraine_posterior %>% 
  group_by(parameter) %>% 
  summarise(calc_metrics(sample))

sleep_hdi <- sleep_posterior %>% 
  group_by(parameter) %>% 
  summarise(calc_metrics(sample))


# plot them

ggplot(migraine_posterior) + 
  geom_density(aes(x = sample, colour = parameter, fill = parameter),
               alpha = .3, size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = hdi_95_min, xmax = hdi_95_max, y = 5),
                 data = migraine_hdi,
                 height = 2) +
  geom_point(aes(x = mean, y = 5),
             data = migraine_hdi) +
  scale_y_continuous(expand = c(0, 0), breaks = NULL) + 
  scale_fill_discrete() + 
  scale_colour_discrete() + 
  labs(x = NULL, y = NULL) +
  facet_wrap(vars(parameter), ncol = 1, scales = "free") +
  theme_prism() +
  theme(legend.position = "none",
        panel.spacing = unit(25, "points"),
        plot.margin = unit(rep(15, 4), "points"),
        axis.line.y = element_blank())

ggsave("Figures/migraine_mod_estimates.png", width = 4, height = 4.5)

ggplot(sleep_posterior) + 
  geom_density(aes(x = sample, colour = parameter, fill = parameter),
               alpha = .3, size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = hdi_95_min, xmax = hdi_95_max, y = 5),
                 data = sleep_hdi,
                 height = 2) +
  geom_point(aes(x = mean, y = 5),
             data = sleep_hdi, ) +
  scale_y_continuous(expand = c(0, 0), breaks = NULL) + 
  scale_fill_discrete() + 
  scale_colour_discrete() + 
  labs(x = NULL, y = NULL) +
  facet_wrap(vars(parameter), ncol = 1, scales = "free") + 
  theme_prism() +
  theme(legend.position = "none",
        panel.spacing = unit(25, "points"),
        plot.margin = unit(rep(15, 4), "points"),
        axis.line.y = element_blank())

ggsave("Figures/sleep_mod_estimates.png", width = 4, height = 3)

