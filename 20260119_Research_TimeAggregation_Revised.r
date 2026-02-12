# Preamble ----
## Packages: Data wrangling
library(dplyr)
library(tidyr)

## Packages: Analysis
library(future)
library(future.apply)
library(data.table)
library(MplusAutomation)
library(ctsem)
library(progressr)

## Packages: Visualization
library(ggplot2)
library(ggh4x)

## Import functions
source("./R/helper.R")
source("./R/Mplus.R")
source("./R/generate.R")
source("./R/ctsem.R")
source("./R/pop_values.R")
source("./R/visualize.R")
source("./R/preprocess.R")
source("./R/summarise.R")

# General ----

## Simulation Factors ----
n_id <- 1
n_t <- 1000
n_burn <- 1000

m <- c(1, 2, 3, 5, 10)
undersample <- c(1, 2, 3, 5, 10)

R <- 1000
future::plan(multisession, workers = 7)

# Scenario 1 ----
# Bivariate setting where X does not change in between measurement occasions and is exogenous.

## Setup ----
# Population values
beta <- c(0, 0.3, 0, 0.5, 0, 0)
delta <- c(int = 0, l = 0, ylag1 = 0, xlag1 = 0, llag1 = 0, llag1Xxlag1 = 0, ylag1Xxlag1 = 0)
gamma <- c(0, 0, 0, 0)
sigma2_l <- 0

# Simulation Factors
conds1 <- expand.grid(m = m, undersample = undersample) |> 
  mutate(
    sigma2_y = compute_error_variance(beta[2], beta[4], p = 0.5), 
    true_lag1k = if_else(m == 1, beta[2]*beta[4]^undersample, NA)
  )

# Models
mods1_ctsem <- list(DTARX = mod_DTARX)
params1_ctsem <- c("A", "Mx")

mods1_Mplus <- "DTLag1X"

## Simulation ----
out1 <- vector("list", nrow(conds1))

for (c in seq_len(nrow(conds1))) {
  out1[[c]] <- run_condition(
    cond_row = conds1[c, ],
    R = R,
    beta = beta,
    delta = delta,
    gamma = gamma,
    x_change_between = FALSE,
    n_id = n_id,
    n_t = n_t,
    n_burn = n_burn,
    sigma2_l = sigma2_l,
    mods_ctsem = mods1_ctsem,
    params_ctsem = params1_ctsem,
    mods_Mplus = mods1_Mplus,
    model_registry = model_registry,
    mplus_templates = mplus_templates,
    base_folder = "./simulation/Scenario1"
  )
}

# Combine all results
out1 <- out1_raw <- do.call(rbind, out1)
saveRDS(out1, file = "./output/Scenario1.rds")

## Results ----

# Restructure true lag1k data.frame for plotting
true1_lag1k <- conds1 |> 
  filter(m == 1) |> 
  select(m, undersample, true_lag1k) |>
  rename(sub = undersample) |> 
  mutate(
    facet_col_fct = as.factor(m), 
    model_lab = "Truth", 
    metric = "avg"
  )

# Preprocess 
out1 <- preprocess1_results(
  results = out1,
  conds = conds1,
  beta  = beta, 
  remove_lag0_models = c("DTARX"),
  par_keep = c("Mx", "BETA_YLag1X", "BETA_YX"),
  compute_ci_models = mods1_Mplus, 
  truth_lag1k = true1_lag1k
)

# Summarize
summ1_long <- compute_performance(data = out1, R = R)

# Scenario 1 Null ----

## Setup ----
# Population values
beta <- c(0, 0, 0, 0.5, 0, 0)

# Simulation Factors
conds1Null <- expand.grid(m = m, undersample = undersample) |> 
  mutate(
    sigma2_y = compute_error_variance(beta[2], beta[4], p = 0.5), 
    pop_lag1k = if_else(m == 1, beta[2]*beta[4]^undersample, NA)
  )

## Simulation ----
out1Null <- vector("list", nrow(conds1Null))

for (c in seq_len(nrow(conds1Null))) {
  out1Null[[c]] <- run_condition(
    cond_row = conds1Null[c, ],
    R = R,
    beta = beta,
    delta = delta,
    gamma = gamma,
    x_change_between = FALSE,
    n_id = n_id,
    n_t = n_t,
    n_burn = n_burn,
    sigma2_l = sigma2_l,
    mods_ctsem = mods1_ctsem,
    params_ctsem = params1_ctsem,
    mods_Mplus = mods1_Mplus,
    model_registry = model_registry,
    mplus_templates = mplus_templates,
    base_folder = "./simulation/Scenario1Null"
  )
}

# Combine all results
out1Null <- out_raw <- do.call(rbind, out1Null)
saveRDS(out1Null, file = "./output/Scenario1Null.rds")

## Results ----

# Restructure true lag1k data.frame for plotting
trueNull_lag1k <- conds1 |> 
  filter(m == 1) |> 
  select(m, undersample) |>
  rename(sub = undersample) |> 
  mutate(
    true_lag1k = 0,
    facet_col_fct = as.factor(m), 
    model_lab = "Truth", 
    metric = "avg"
  )

# Preprocess 
out1Null <- preprocess1_results(
  results = out1Null,
  conds = conds1Null,
  beta  = beta, 
  remove_lag0_models = c("DTARX"),
  par_keep = c("Mx", "BETA_YLag1X", "BETA_YX"),
  compute_ci_models = mods1_Mplus, 
  truth_lag1k = trueNull_lag1k
)

# Summarize
summ1Null_long <- compute_performance(data = out1Null, R = R)

# Scenario 2 ----

## Setup ----
# Population values
beta <- c(0, 0.3, 0, 0.5, 0, 0)
delta <- c(int = 0, l = 0, ylag1 = 0.5, xlag1 = 0.2, llag1 = 0, llag1Xxlag1 = 0, ylag1Xxlag1 = -0.3)

# Simulation Factors
conds2 <- expand.grid(m = m, undersample = undersample) 

# Approximate error variance due to effects on Y via binary X
conds2$sigma2_y <- approximate_error_variance(beta = beta, delta = delta,
  gamma = gamma, m = 1, undersample = 1, x_change_between = FALSE
)[[3]]$y

# Models
mods2_ctsem <- NULL
params2_ctsem <- c("A", "Mx")

mods2_Mplus <- c("DTLag1X", "DTLag1XMulti")

## Simulation ----
out2 <- vector("list", nrow(conds2))

for (c in seq_len(nrow(conds2))) { # Loop over conditions
  out2[[c]] <- run_condition(
    cond_row = conds2[c, ],
    R = R,
    beta = beta,
    delta = delta,
    gamma = gamma,
    x_change_between = FALSE,
    n_id = n_id,
    n_t = n_t,
    n_burn = n_burn,
    sigma2_l = sigma2_l,
    mods_ctsem = mods2_ctsem,
    params_ctsem = params2_ctsem,
    mods_Mplus = mods2_Mplus,
    model_registry = model_registry,
    mplus_templates = mplus_templates,
    base_folder = "./simulation/Scenario2"
  )
}

# Combine all results
out2 <- out_raw <- do.call(rbind, out2)
saveRDS(out2, file = "./output/Scenario2.rds")

## Results ----
true2_lag1k <- extract_simulated_true_lag1k(dat = out2)

# Preprocess 
out2 <- preprocess1_results(
  results = out2,
  conds = conds2,
  beta = beta, 
  par_keep = c("Mx", "BETA_YLag1X", "BETA_YX", "BETA_TotLag1X"),
  remove_lag0_models = c("DTARX", "DTBARLag0X"),
  compute_ci_models = c("DTARLag1X", "DTSBAR", "DTLag1X"), 
  truth_lag1k = true2_lag1k
)

# Summarize
summ2_long <- compute_performance(data = out2, R = R)

# Scenario 2 Null ----
# Bivariate setting where X does not change in between measurement occasions and is endogenous

## Setup ----
# Population values
beta <- c(0, 0, 0, 0.5, 0, 0)

# Simulation Factors
conds2Null <- expand.grid(m = m, undersample = undersample) |> 
  mutate(
    sigma2_y = compute_error_variance(beta[2], beta[4], p = 0.5), 
    pop_lag1k = if_else(m == 1, beta[2]*beta[4]^undersample, NA)
  )

## Simulation ----
out2Null <- vector("list", nrow(conds2Null))

for (c in seq_len(nrow(conds2Null))) { # Loop over conditions
  out2Null[[c]] <- run_condition(
    cond_row = conds2[c, ],
    R = R,
    beta = beta,
    delta = delta,
    gamma = gamma,
    x_change_between = FALSE,
    n_id = n_id,
    n_t = n_t,
    n_burn = n_burn,
    sigma2_l = sigma2_l,
    mods_ctsem = mods2_ctsem,
    params_ctsem = params2_ctsem,
    mods_Mplus = mods2_Mplus,
    model_registry = model_registry,
    mplus_templates = mplus_templates,
    base_folder = "./simulation/Scenario2Null"
  )
}

# Combine all results
out2Null <- out_raw <- do.call(rbind, out2Null)
saveRDS(out2Null, file = "./output/Scenario2Null.rds")

## Results ----

# Preprocess 
out2Null <- preprocess1_results(
  results = out2Null,
  conds = conds2Null,
  beta = beta, 
  par_keep = c("Mx", "BETA_YLag1X", "BETA_YX", "BETA_TotLag1X"),
  remove_lag0_models = c("DTARX", "DTSBAR"),
  compute_ci_models = c("DTARLag1X", "DTSBAR", "DTLag1X"), 
  truth_lag1k = trueNull_lag1k
)

# Summarize
summ2Null_long <- compute_performance(data = out2Null, R = R)

# Scenario 3 ----
# Bivariate setting where X can change in between measurement occasions and is endogenous

## Setup ----
# Population values
beta <- c(0, 0.3, 0, 0.5, 0, 0)
delta <- c(int = 0, l = 0, ylag1 = 0.5, xlag1 = 0.2, llag1 = 0, llag1Xxlag1 = 0, ylag1Xxlag1 = -0.3)

conds3 <- expand.grid(m = m, undersample = undersample) 

conds3$sigma2_y <- approximate_error_variance(beta = beta, delta = delta,
  gamma = gamma, m = 1, undersample = 1, x_change_between = TRUE
)[[3]]$y

# Models
mods3_ctsem <- list(DTARX = mod_DTARX)
params3_ctsem <- c("A", "Mx")

mods3_Mplus <- c("DTBAR", "DTLag1X", "DTSBAR", "DTARLag1X", "DTLag1XMulti") 

## Simulation ----
out3 <- vector("list", nrow(conds3))

for (c in seq_len(nrow(conds3))) {
  out3[[c]] <- run_condition(
    cond_row = conds3[c, ],
    R = R,
    beta = beta,
    delta = delta,
    gamma = gamma,
    x_change_between = TRUE,
    n_id = n_id,
    n_t = n_t,
    n_burn = n_burn,
    sigma2_l = sigma2_l,
    mods_ctsem = mods3_ctsem,
    params_ctsem = params3_ctsem,
    mods_Mplus = mods3_Mplus,
    model_registry = model_registry,
    mplus_templates = mplus_templates,
    base_folder = "./simulation/Scenario3"
  )
}

# Combine all results
out3 <- out_raw <- do.call(rbind, out3)
saveRDS(out3, file = "./output/Scenario3.rds")

## Results ----
# Extract simulated true empirical lag1 effect
true3_lag1k <- extract_simulated_true_lag1k(dat = out3)

# Preprocess 
out3 <- preprocess1_results(
  results = out3,
  conds = conds3,
  beta = beta, 
  remove_lag0_models = character(0),
  par_keep = c("Mx", "BETA_YLag1X", "BETA_YX", "BETA_TotLag1X"),
  compute_ci_models = mods3_Mplus, 
  truth_lag1k = true3_lag1k
)

# Summarize
summ3_long <- compute_performance(data = out3, R = R)

# Scenario 3 Null ----

## Setup ----
# Population values
beta <- c(0, 0.3, 0, 0, 0, 0)
delta <- c(int = 0, l = 0, ylag1 = 0, xlag1 = 0, llag1 = 0, llag1Xxlag1 = 0, ylag1Xxlag1 = 0)

conds3Test <- expand.grid(m = m, undersample = undersample) |> 
  mutate(
    sigma2_y = compute_error_variance(beta[2], beta[4], p = 0.5), 
    pop_lag1k = if_else(m == 1, beta[2]*beta[4]^undersample, NA)
  )

## Simulation ----
out3Test <- vector("list", nrow(conds3Test))

for (c in seq_len(nrow(conds3Test))) {
  out3Test[[c]] <- run_condition(
    cond_row = conds3Test[c, ],
    R = R,
    beta = beta,
    delta = delta,
    gamma = gamma,
    x_change_between = TRUE,
    n_id = n_id,
    n_t = n_t,
    n_burn = n_burn,
    sigma2_l = sigma2_l,
    mods_ctsem = mods3_ctsem,
    params_ctsem = params3_ctsem,
    mods_Mplus = mods3_Mplus,
    model_registry = model_registry,
    mplus_templates = mplus_templates,
    base_folder = "./simulation/Scenario3Test"
  )
}

# Combine all results
out3Test <- out_raw <- do.call(rbind, out3Test)
saveRDS(out3Test, file = "./output/Scenario3Test.rds")

## Results ----

# Preprocess 
out3Test <- preprocess1_results(
  results = out3Test,
  conds = conds3Test,
  beta = beta, 
  remove_lag0_models = character(0),
  par_keep = c("Mx", "BETA_YLag1X", "BETA_YX"),
  compute_ci_models = c("DTLag1X", "DTSBAR", "DTBAR"),
  truth_lag1k = trueNull_lag1k
)

# Summarize
summ3Null_long <- compute_performance(data = out3Test, R = R)

# Scenario 4 ----
# Trivariate setting where X can change in between measurement occasions and there 
# exists a time-varying confounder L. 

## Setup ----
# Population values
beta <- c(0, 0.3, 0.2, 0.5, 0, 0)
delta <- c(int = 0, l = -1, ylag1 = 0.5, xlag1 = 0.2, llag1 = 0, llag1Xxlag1 = 0, ylag1Xxlag1 = 0.3)
gamma <- c(0, 0, 0, 0.4)

conds4 <- expand.grid(m = m, undersample = undersample) 

conds4$sigma2_y <- approximate_error_variance(beta = beta, delta = delta,
  gamma = gamma, m = 1, undersample = 1, x_change_between = TRUE
)[[3]]$y

# Models
mods4_ctsem <- NULL
params4_ctsem <- c("A", "Mx", "Ml")

mods4_Mplus <- c("DTLag1XL", "DTLag1XLMulti") 

## Simulation ----
out4 <- vector("list", nrow(conds4))

for (c in seq_len(nrow(conds4))) {
  out4[[c]] <- run_condition(
    cond_row = conds4[c, ],
    R = R,
    beta = beta,
    delta = delta,
    gamma = gamma,
    x_change_between = TRUE,
    n_id = n_id,
    n_t = n_t,
    n_burn = n_burn,
    sigma2_l = sqrt(0.84),
    mods_ctsem = mods4_ctsem,
    params_ctsem = params4_ctsem,
    mods_Mplus = mods4_Mplus,
    model_registry = model_registry,
    mplus_templates = mplus_templates,
    base_folder = "./simulation/Scenario4"
  )
}

# Combine all results
out4 <- out_raw <- do.call(rbind, out4)
saveRDS(out4, file = "./output/Scenario4.rds")

## Results ----

# Preprocess 
out4 <- preprocess1_results(
  results = out4,
  conds = conds4,
  beta = beta, 
  remove_lag0_models = character(0),
  par_keep = c("Mx", "BETA_YLag1X", "BETA_YX"),
  compute_ci_models = mods4_Mplus, 
  truth_lag1k = true3_lag1k # The same as in Scenario 3
)

# Summarize
summ4_long <- compute_performance(data = out4, R = R)

# Scenario 4 Null ----

## Setup ----
# Population values
beta <- c(0, 0, 0.2, 0.5, 0, 0)

conds4Null <- expand.grid(m = m, undersample = undersample) 

conds4Null$sigma2_y <- approximate_error_variance(n_t = 1e4, beta = beta, delta = delta,
  gamma = gamma, m = 1, undersample = 1, x_change_between = TRUE
)[[3]]$y


## Simulation ----
out4Null <- vector("list", nrow(conds4Null))

for (c in seq_len(nrow(conds4Null))) {
  out4Null[[c]] <- run_condition(
    cond_row = conds4Null[c, ],
    R = R,
    beta = beta,
    delta = delta,
    gamma = gamma,
    x_change_between = TRUE,
    n_id = n_id,
    n_t = n_t,
    n_burn = n_burn,
    sigma2_l = sqrt(0.84),
    mods_ctsem = mods4_ctsem,
    params_ctsem = params4_ctsem,
    mods_Mplus = mods4_Mplus,
    model_registry = model_registry,
    mplus_templates = mplus_templates,
    base_folder = "./simulation/Scenario4Null"
  )
}

# Combine all results
out4Null <- out_raw <- do.call(rbind, out4Null)
saveRDS(out4Null, file = "./output/Scenario4Null.rds")

## Results ----

# Preprocess 
out4Null <- preprocess1_results(
  results = out4Null,
  conds = conds4Null,
  beta = beta, 
  remove_lag0_models = character(0),
  par_keep = c("Mx", "BETA_YLag1X", "BETA_YX"),
  compute_ci_models = mods4_Mplus, 
  truth_lag1k = trueNull_lag1k
)

# Summarize
summ4Null_long <- compute_performance(data = out4Null, R = R)

# Scenario 5 ----
# Panel data setting

## Setup ----
n_id <- 500
n_t <- 5
n_burn <- 1000

# Population values
beta <- c(0, 0.3, 0.2, 0.5, 0, 0)
delta <- c(int = 0, l = -1, ylag1 = 0.5, xlag1 = 0.2, llag1 = 0, llag1Xxlag1 = 0, ylag1Xxlag1 = 0.3)
gamma <- c(0, 0, 0, 0.4)

conds5 <- data.frame(
  m = c(1, 7, 30, 365, 7), 
  undersample = c(1, 4, 12, 1, 102)
) 

conds5$sigma2_y <- approximate_error_variance(beta = beta, delta = delta,
  gamma = gamma, m = 1, undersample = 1, x_change_between = TRUE
)[[3]]$y

# Models
mods5_Mplus <- c("DTCLPM", "DTCLPMLag0")

## Simulation ----
out5 <- vector("list", nrow(conds5))

for (c in seq_len(nrow(conds5))) {
  out5[[c]] <- run_condition(
    cond_row = conds5[c, ],
    R = R,
    beta = beta,
    delta = delta,
    gamma = gamma,
    x_change_between = TRUE,
    n_id = n_id,
    n_t = n_t,
    n_burn = n_burn,
    sigma2_l = sqrt(0.84),
    mods_ctsem = NULL,
    params_ctsem = NULL,
    mods_Mplus = mods5_Mplus,
    model_registry = model_registry,
    mplus_templates = mplus_templates,
    base_folder = "./simulation/Scenario5", 
    model_wide = TRUE
  )
}

# Combine all results
out5 <- out_raw <- do.call(rbind, out5)
saveRDS(out5, file = "./output/Scenario5.rds")

## Results ----

# Preprocess 
out5 <- preprocess1_results(
  results = out5,
  conds = conds5,
  beta = beta, 
  remove_lag0_models = character(0),
  par_keep = c(
    "BETA_Y5X4", "BETA_Y4X3", "BETA_Y3X2", "BETA_Y2X1",
    "BETA_Y5X5", "BETA_Y4X4", "BETA_Y3X3", "BETA_Y2X2"
  ),
  compute_ci_models = mods5_Mplus, 
  truth_lag1k = true_ILD
)

# Summarize
summ5_long <- compute_performance(data = out5, R = R)
res5 <- summ5_long |> 
  filter(
    par %in% c("BETA_Y5X4", "BETA_Y4X4") &
      metric %in% c("avg", "rejectionH0")
  )
res5

# Visualize ----

# Combine results across scenarios
results_ILD <- bind_rows(
  list(
    `1` = summ1_long, `1 Null` = summ1Null_long, 
    `2` = summ2_long, `2 Null` = summ2Null_long,
    `3` = summ3_long, `3 Null` = summ3Null_long, 
    `4` = summ4_long, `4 Null` = summ4Null_long
  ),
  .id = "scenario"
) |> 
  mutate(
    model_type = case_when(
      model %in% c("DTARX", "DTARXL", "DTLag1X", "DTLag1XL") ~ "univariate",
      model %in% c("DTSBAR", "DTSVAR", "DTLag1XMulti", "DTLag1XLMulti") ~ "Multivariate"
    )
  )

## Scenario 1 ----
plot1_lag0 <- make_par_plot(
  data = results_ILD,
  par_select = "lag0",
  model_select = c("DTARX"),
  scenario_select = c("1", "1 Null"), 
  x_var = "sub", 
  x_lab = "Undersample",
  facet_col_var = "m",
  metric_labs = c(bias = "Bias", rejectionH0 = "Power / Type I Error"),
  y_scales = NULL,
  hlines = tibble(
    metric = c("bias", "rejectionH0"),
    yintercept = c(0, 0.05)
  ),
  save_path = "./figures/plot1_lag0.png"
)

plot1_lag1k <- make_par_plot(
  data = results_ILD,
  par_select = "lag1k",
  model_select = c("DTLag1X"),
  scenario_select = c("1", "1 Null"), 
  x_var = "sub", 
  x_lab = "Undersample",
  facet_col_var = "m",
  metric_labs = c(avg = "Avg.", rejectionH0 = "Power / Type I Error"),
  y_scales = NULL,
  hlines = tibble(
    metric = c("avg", "rejectionH0"),
    yintercept = c(0, 0.05)
  ),
  save_path = "./figures/plot1_lag1k.png", 
  truth = true1_lag1k
)

## Scenario 2 ----
plot2_lag0 <- make_par_plot(
  data = results_ILD,
  par_select = "lag0",
  model_select = c("DTARX", "DTSBAR"),
  scenario_select = c("2", "2 Null"), 
  x_var = "sub", 
  x_lab = "Undersample",
  facet_col_var = "m",
  metric_labs = c(bias = "Bias", rejectionH0 = "Power / Type I Error"),
  y_scales = NULL,
  hlines = tibble(
    metric = c("bias", "rejectionH0"),
    yintercept = c(0, 0.05)
  ),
  save_path = "./figures/plot2_lag0.png"
)

plot2_lag1k <- make_par_plot(
  data = results_ILD,
  par_select = "lag1k",
  model_select = c("DTLag1X", "DTLag1XMulti"),
  scenario_select = c("2", "2 Null"), 
  x_var = "sub", 
  x_lab = "Undersample",
  facet_col_var = "m",
  metric_labs = c(avg = "Avg.", rejectionH0 = "Power / Type I Error"),
  y_scales = NULL,
  hlines = tibble(
    metric = c("avg", "rejectionH0"),
    yintercept = c(0, 0.05)
  ),
  save_path = "./figures/plot2_lag1k.png", 
  truth = true2_lag1k
)

## Scenario 3 ----
plot3_lag0 <- make_par_plot(
  data = results_ILD,
  par_select = "lag0",
  model_select = c("DTARX", "DTSBAR"),
  scenario_select = c("3", "3 Null"), 
  x_var = "sub", 
  x_lab = "Undersample",
  facet_col_var = "m",
  metric_labs = c(bias = "Bias", rejectionH0 = "Power / Type I Error"),
  y_scales = NULL,
  hlines = tibble(
    metric = c("bias", "rejectionH0"),
    yintercept = c(0, 0.05)
  ),
  save_path = "./figures/plot3_lag0.png"
)

plot3_lag0 <- make_par_plot(
  data = results_ILD,
  par_select = "lag1k",
  model_select = c("DTLag1X", "DTLag1XMulti"),
  scenario_select = c("3", "3 Null"), 
  x_var = "sub", 
  x_lab = "Undersample",
  facet_col_var = "m",
  metric_labs = c(bias = "Bias", rejectionH0 = "Power / Type I Error"),
  y_scales = NULL,
  hlines = tibble(
    metric = c("bias", "rejectionH0"),
    yintercept = c(0, 0.05)
  ),
  save_path = "./figures/plot3_lag1k.png"
)


## Scenario 4 ----
plot4_lag0 <- make_par_plot(
  data = results_ILD,
  par_select = "lag0",
  model_select = c("DTARXL", "DTSVAR"),
  scenario_select = c("4", "4 Null"), 
  x_var = "sub", 
  x_lab = "Undersample",
  facet_col_var = "m",
  metric_labs = c(bias = "Bias", rejectionH0 = "Power / Type I Error"),
  y_scales = NULL,
  hlines = tibble(
    metric = c("bias", "rejectionH0"),
    yintercept = c(0, 0.05)
  ),
  save_path = "./figures/plot4_lag0.png"
)

plot4_lag1k <- make_par_plot(
  data = results_ILD,
  par_select = "lag1k",
  model_select = c("DTLag1XL", "DTLag1XLMulti"),
  scenario_select = c("4", "4 Null"), 
  x_var = "sub", 
  x_lab = "Undersample",
  facet_col_var = "m",
  metric_labs = c(avg = "Avg.", rejectionH0 = "Power / Type I Error"),
  y_scales = NULL,
  hlines = tibble(
    metric = c("avg", "rejectionH0"),
    yintercept = c(0, 0.05)
  ),
  save_path = "./figures/plot4_lag1k.png"
)
