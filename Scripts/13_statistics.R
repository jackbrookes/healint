correlation_matrices <- combined_user_summary %>% 
  select(painintensity:num_interruptions, -frequency, -age_category) %>% 
  as.matrix() %>% 
  rcorr() %>% 
  map(~as_tibble(., rownames = "column"))

correlation_matrices$P %>% 
  write_csv("Output/correlation_p_values.csv")

correlation_matrices$r %>% 
  write_csv("Output/correlation_R_values.csv")
