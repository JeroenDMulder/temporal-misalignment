#' Generate Formula OLS-Estimate Aggregated AR(1) Process
#' 
#' `compute_Ahat()` computes the OLS-estimate for the autoregressive parameter of an AR(1) process when aggregating of `n` time-periods. 
#'
#' @param n An integer, the number of time-periods to aggregate over. 
create_AHat_formula <- function(n) {
  
  # For disaggregated data (n = 1)
  if (n == 1) { return("a") }
  
  # Generate coefficient pattern
  half_n <- ceiling(n)
  coef_numerator <- c(1:half_n, (half_n - 1):1)
  coef_denominator <- sapply(1:(n - 1), function(i) {2*n - 2 * i})
  
  # Generate polynomial terms
  numerator_terms <- paste0(coef_numerator, "*a^", 1:(2*n - 1))
  denominator_terms <- paste0(coef_denominator, "*a^", 1:(n - 1))
  
  # Collapse into single string
  formula_numerator <- paste(numerator_terms, collapse = " + ")
  formula_denominator <- paste(denominator_terms, collapse = " + ") |>
    paste("+", paste(n))
  
  # Create formula
  formula_str <- paste0("(", formula_numerator, ") / (", formula_denominator, ")")
  return(formula_str)
}

#' Evaluate Formula OLS-Estimate Aggreated AR(1)
#' 
#' @param n An integer, the number of time-periods to aggregate over.
#' @param a_value A double, the autoregressive effect of the disaggregated AR(1) process.
evaluate_AHat <- function(n, a_value) {
  
  # Generate formula
  formula_str <- create_AHat_formula(n)
  
  # Replace 'a' with 'a_value'
  formula_with_value <- gsub("a", as.character(a_value), formula_str)
  
  # Evaluate formula
  out <- eval(parse(text = formula_with_value))
  return(out)
}

# Compute AHat for range of n
n_range <- 1:100
df_AHat_nToInf <- data.frame(
  n = n_range, 
  AHat = sapply(n_range, evaluate_AHat, a_value = 0.5)
)

# Plot AHat across n
plot_AHat_nToInf <- ggplot(df_AHat_nToInf, aes(x = n, y = AHat)) + 
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  ylim(c(0, 0.5)) + 
  labs(x = "n", y = expression(hat(A))) + 
  theme_classic()

ggsave(
  filename = "./figures/plot_AHat_nToInf.png", 
  plot = plot_AHat_nToInf, 
  width = 10, 
  height = 5
)


