
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


# ordinal linear regression to predict frequency from sleep
combined_user_summary <- combined_user_summary %>% 
  mutate(frequency = as.factor(frequency))

hours_slept_model <- polr(frequency ~ total_hours_slept, data = combined_user_summary, Hess=TRUE)

## view a summary of the model
summary(hours_slept_model)
(ctable <- coef(summary(hours_slept_model)))

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

exp(coef(hours_slept_model))

newdat <- combined_user_summary %>% 
  select(frequency, total_hours_slept, hashed_userid)

newdat <- cbind(newdat, predict(hours_slept_model, newdat, type = "probs"))
head(newdat)

lnewdat <- melt(newdat, id.vars = c("total_hours_slept"),
                variable.name = "frequency", value.name="Probability")
                
## view first few rows

head(lnewdat)
ggplot(lnewdat, aes(x = total_hours_slept, y = Probability, colour = frequency)) +
  geom_line() + facet_grid(total_hours_slept ~ frequency)
