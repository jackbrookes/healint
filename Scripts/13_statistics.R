
# Compute correlation matrix between all relevant variables and save R and p-values to csv

correlation_matrices <- combined_user_summary %>% 
  select(painintensity:num_interruptions, -frequency, -age_category) %>% 
  as.matrix() %>% 
  rcorr() %>% 
  map(~as_tibble(., rownames = "column"))

correlation_matrices$P %>% 
  write_csv("Output/correlation_p_values.csv")

correlation_matrices$r %>% 
  write_csv("Output/correlation_R_values.csv")

# generate correlation matrix for plotting pretty correlogram


corr_mat <- combined_user_summary %>% 
  select(painintensity:num_interruptions,
         -frequency,
         -age_category, 
         -total_pain, 
         -trigger_menstruation,
         -migraine_start_time_from_midnight, 
         -migraine_end_time_from_midnight, 
         -start_time_from_midnight, 
         -attacks,
         -end_time_from_midnight) %>% 
  as.matrix()

corr_result <-rcorr(corr_mat)

# function to generate p-values to plot

cor_test <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  pvalue_matrix <- matrix(NA, n, n)
  diag(pvalue_matrix ) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      pvalue_matrix [i, j] <- pvalue_matrix [j, i] <- tmp$p.value
    }
  }
  colnames(pvalue_matrix ) <- rownames(pvalue_matrix ) <- colnames(mat)
  pvalue_matrix 
}
# matrix of the p-value of the correlation

pvalue_matrix <- combined_user_summary %>% 
  select(painintensity:num_interruptions,
         -frequency,
         -age_category, 
         -total_pain, 
         -trigger_menstruation,
         -migraine_start_time_from_midnight, 
         -migraine_end_time_from_midnight, 
         -start_time_from_midnight, 
         -attacks,
         -end_time_from_midnight) %>% 
  cor_test()

head(pvalue_matrix[, 1:5])

# plot correlogram - tada!

corrplot(corr_result$r, type = "lower", p.mat =  p.mat, sig.level = 0.05, 
         method = "number", 
         order="hclust",
         col=brewer.pal(n=8, name="Spectral"), 
         insig = "blank",
         addCoef.col = "black",
         tl.col="black", tl.srt=45, 
         diag=FALSE)


