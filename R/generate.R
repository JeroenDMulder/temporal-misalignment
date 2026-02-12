#' Simulate ARX Process with Time-Varying Confounder L
#'
#' @param n_id Number of units.
#' @param n_t Number of time points to keep after aggregation and undersampling. 
#' @param n_burn Number of time point to discard as burn-in. 
#' @param beta Vector of regression parameters for Y. The parameters are ordered from predictor nearest to furthest in time; the first element is the intercept. 
#' @param delta List of vectors with regression parameters for X. The list is ordered (state 0, state 1); parameter vectors are ordered from predictor nearest to furthest in time. 
#' @param gamma Vector of regression parameters for L. The parameters are ordered from predictor nearest to furthest in time; the first element is the intercept. 
#' @param sigma2_y Residual variance of Y.
#' @param sigma2_l Residual variance of L.
#' @param m Integer denoting the number of time points to aggregate over.
#' @param undersample Integer denoting 
#' @param x_change_between Boolean denoting whether treatment `X` changes in between measurement occasions. If `FALSE`, there is an immediate effect of `X` only at the measurement occasions directly, even when other variable do vary between measurement occasions. 
#'
#' @example 
#' df <- simulate_ARXL(
#'   n_id = 1,
#'   n_t = 2,
#'   n_burn = 4,
#'   beta = c(0, 2, 0, 0, 0, 0),
#'   delta = list(int = c(0, 0), lt = c(0, 0), ylag1 = c(0, 0), xlag1 = c(0, 0), llag1 = c(0, 0)),
#'   gamma = c(0, 0, 0, 0),
#'   sigma2_y = 0.74,
#'   sigma2_l = 0,
#'   m = 1,
#'   undersample = 1,
#'   x_change_between = FALSE,
#'   debug = TRUE
#' )
simulate_ARXL <- function(
    n_id, n_t, n_burn, 
    beta, delta, gamma, sigma2_y, sigma2_l,
    m, undersample, 
    x_change_between
  ) {

  # Adjust burn-in period to match m * undersample interval
  if (n_burn %% (m * undersample) != 0) {
    n_burn <- n_burn + (m * undersample) - (n_burn %% (m * undersample))
  }
  
  # Time points
  n_t_adjusted <- n_t * m * undersample
  TT <- n_t_adjusted + n_burn
  
  if (!x_change_between) {
    # Create indicator for when X can change (i.e., only at measurement occasions)
    idx_x_change <- seq(from = m * undersample, TT, by = m*undersample) 
    I_x_change <- rep(0, TT)
    I_x_change[idx_x_change] <- 1
  } else {
    I_x_change <- rep(1, TT)
  }
  
  # Preallocate memory
  y <- l <- x <- matrix(0, nrow = TT, ncol = n_id)
  
  # Sample exogenous variables
  e_y <- matrix(rnorm(n_id * TT, 0, sqrt(sigma2_y)), nrow = TT, ncol = n_id)
  e_l <- matrix(rnorm(n_id * TT, 0, sqrt(sigma2_l)), nrow = TT, ncol = n_id)
  
  # Time 2 and later
  for (t in 2:TT) { 
    
    # Model L
    l[t, ] <- gamma[1] + gamma[2] * y[t - 1, ] + gamma[3] * x[t - 1, ] + gamma[4] * l[t - 1, ] + e_l[t, ]
    
    # Model X
    if (I_x_change[t] == 1) {
      p <- delta["int"] + 
        delta["l"] * l[t, ] + 
        delta["ylag1"] * y[t - 1, ] + 
        delta["xlag1"] * x[t - 1, ] + 
        delta["llag1"] * l[t - 1, ] + 
        delta["ylag1Xxlag1"] * y[t - 1, ] * x[t - 1, ] + 
        delta["llag1Xxlag1"] * l[t - 1, ] * x[t - 1, ] 
        
      x[t, ] <- rbinom(n_id, 1, plogis(p))
    } else {
      x[t, ] <- x[t - 1, ]
    }
    
    # Model Y
    y[t, ] <- beta[1] + (I_x_change[t] * beta[2]) * x[t, ] + beta[3] * l[t, ] + 
      beta[4] * y[t - 1, ] + beta[5] * x[t - 1, ] + beta[6] * l[t - 1, ] + 
      e_y[t, ]
  }
  
  # Remove burnin
  x <- x[(n_burn + 1):TT, ]
  l <- l[(n_burn + 1):TT, ]
  y <- y[(n_burn + 1):TT, ]
  
  # Aggregate
  l_m <- matrix(
    colMeans(array(l, dim = c(m, n_t_adjusted / m, n_id))),
    nrow = n_t_adjusted / m, 
    ncol = n_id
  )
    
  if (!x_change_between & m > 1) {
    
    # Indicator (without burnin period) for when X affects Y / changes
    idx_x_m <- seq(m, n_t_adjusted, by = m)
    
    # Sample last X from each alignment interval
    x_m <- if(n_id == 1) {x[idx_x_m]} else {x[idx_x_m, ]}
    x_m <- matrix(x_m, nrow = n_t_adjusted / m, ncol = n_id)
  
    } else {
    x_m <- matrix(
      colMeans(array(x, dim = c(m, n_t_adjusted / m, n_id))),
      nrow = n_t_adjusted / m, 
      ncol = n_id
    )
  }
    
  y_m <- matrix(
    colMeans(array(y, dim = c(m, n_t_adjusted / m, n_id))),
    nrow = n_t_adjusted / m, 
    ncol = n_id
  )
  
  # Index undersampling (after removing burning and aggregation)
  idx_sub <- seq(undersample, n_t*undersample, by = undersample)
  
  l_sub <- l_m[idx_sub, , drop = FALSE]
  x_sub <- x_m[idx_sub, , drop = FALSE]
  y_sub <- y_m[idx_sub, , drop = FALSE]
    
  # Create dataframe
  df <- data.frame(
    id = rep(seq_len(n_id), each = n_t), 
    time = rep(seq_len(n_t), times = n_id), 
    y = as.vector(y_sub), 
    x = as.vector(x_sub),
    l = as.vector(l_sub)
  )
  
  return(df)
}


add_lag0_time <- function(dat, var, offset) {
  
  n <- nrow(dat)
  
  # Duplicate rows
  out <- dat[rep(seq_len(n), each = 2), ]
  
  # Indices
  early <- seq(1, 2 * n, by = 2)
  late <- seq(2, 2 * n, by = 2)
  
  # Adjust time
  out$time[early] <- out$time[early] - offset
  
  # Variables not shifted
  other_vars <- setdiff(names(dat), c(var, "time"))
  
  # Change early and late variables to NA
  for (v in other_vars) {out[early, v] <- NA}
  for (v in var) {out[late, v] <- NA}
  
  rownames(out) <- NULL
  out
}
