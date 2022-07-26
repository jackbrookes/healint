# Recode symptoms

symptoms_recoded <- symptoms_raw %>%
  group_by(replacement, classification) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(replacement = fct_reorder(replacement, n)) %>% 
  arrange(replacement)
