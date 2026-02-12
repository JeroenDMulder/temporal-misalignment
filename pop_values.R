# Preamble ----
library(dplyr)

#' Compute Residual Variance for ARX
#'
#' @param beta2 Double denoting the regression coefficient for the lag-0 effect of the exogenous predictor `X`. 
#' @param beta4 Double denoting the lag-1 autoregressive parameter of the outcome. 
#' @param p Double denoting the probability of success for Bernoulli-distributed `X` at measurement occasion.
#' @inheritParams generate
compute_error_variance <- function(beta2, beta4, p) {
  # Compute first two moments of X
  E_x <- p 
  Var_x <- E_x * (1 - E_x)
  
  # Compute residual variance
  sigma2_e <- (1 - beta4^2) - beta2^2 * Var_x
  return(sigma2_e)
}

#' Approximate Residual Error Variances Through Simulation
#' 
#' For nonlinear processes, there does not exist a closed-form solution for determining the size of the residual variances such that the population value regression coefficients can be interpreted as standardized. The function approximates the the residual variances via simulations. 
#' @inheritParams generate
approximate_error_variance <- function(
    n_id = 100, 
    n_t = 1e4, 
    beta, delta, gamma, 
    m, undersample,
    x_change_between
  ) {
  
  # Simulate variance in outcomes for unit error variances
  vars_unit_error <- simulate_ARXL(
    n_id = n_id, 
    n_t = n_t, 
    n_burn = 1e3, 
    beta = beta,
    delta = delta,
    gamma = gamma, 
    sigma2_y = 1, 
    sigma2_l = 1,
    m = m, undersample = undersample, 
    x_change_between = x_change_between
  ) |> 
    dplyr::select(y, l, x) |> 
    var()
  
  # Compute adjusted error variances
  sigma2_y_adjusted <- 1 / vars_unit_error["y", "y"]
  sigma2_l_adjusted <- 1 / vars_unit_error["l", "l"]
  
  # Check variance in outcomes for adjusted error variances
  vars_adjusted_error <- simulate_ARXL(
    n_id = n_id, 
    n_t = n_t, 
    n_burn = 1e3, 
    beta = beta,
    delta = delta,
    gamma = gamma, 
    sigma2_y = sigma2_y_adjusted, 
    sigma2_l = sigma2_l_adjusted,
    m = m, undersample = undersample,
    x_change_between
  ) |> 
    dplyr::select(y, l, x) |> 
    var()
  
  # Compute measures of interest
  beta2_stdyx <- beta[2] * (sqrt(vars_adjusted_error["x", "x"]) / sqrt(vars_adjusted_error["y", "y"]))
  r2_beta2 <- beta[2]^2 * vars_adjusted_error["x", "x"]
  df_sigma2_adjust <- data.frame(
    y = sigma2_y_adjusted,
    l = sigma2_l_adjusted
  )
  
  list(
    beta2_stdyx = beta2_stdyx, 
    r2_beta2 = r2_beta2, 
    df_sigma2_adjust = df_sigma2_adjust
  )
}
