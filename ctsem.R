# Preamble ----
library(ctsem)
library(data.table)

# Models ----

## No Control for Time-Varying L ----

### Univariate ----
mod_DTARX <- ctModel(
  type = "dt",
  id = "id",
  time = "time",
  n.manifest = 1,
  manifestNames = "y",
  n.latent = 1,
  latentNames = "etaY",
  n.TDpred = 1,
  TDpredNames = matrix("x"),  
  LAMBDA = matrix(1),
  MANIFESTVAR = matrix(0),
  MANIFESTMEANS = matrix(0),
  DRIFT = matrix("A"), 
  CINT = matrix(0),  
  T0MEANS = matrix(0), 
  T0VAR = matrix(1),      
  TDPREDEFFECT = matrix("Mx"),
  silent = TRUE
)

# fit <- ctStanFit(dat = df, ctstanmodel = mod_DTARX, iter = 2000, chains = 2)
# summary(fit)

mod_CTARX <- ctModel(
  type = "ct",
  id = "id",
  time = "time",
  n.manifest = 1,
  manifestNames = "y",
  n.latent = 1,
  latentNames = "etaY",
  n.TDpred = 1,
  TDpredNames = matrix("x"),  
  LAMBDA = matrix(1),
  MANIFESTVAR = matrix(0),
  MANIFESTMEANS = matrix(0),
  DRIFT = matrix("A"), 
  CINT = matrix(0),  
  T0MEANS = matrix(0), 
  T0VAR = matrix(1),      
  TDPREDEFFECT = matrix("Mx"),
  silent = TRUE
)

# fit <- ctStanFit(dat = df, ctstanmodel = mod_DTARX, iter = 2000, chains = 2)
# summary(fit)

### Bivariate ----
mod_DTBAR <- ctModel( # 
  type = "dt",
  id = "id",
  time = "time",
  n.manifest = 2,
  manifestNames = c("y", "x"),
  manifesttype = c(0, 1),
  n.latent = 2,
  latentNames = c("etaY", "etaX"),
  n.TDpred = 0,
  LAMBDA = diag(2),
  MANIFESTVAR = matrix(c(0, 0, 0, "thetax"), nrow = 2),
  MANIFESTMEANS = matrix(c(0, 0), nrow = 2),
  DRIFT = c("auto"), 
  CINT = matrix(c(0, "cintx"), nrow = 2),  
  T0MEANS = c(0, 0), 
  T0VAR = c("auto"),      
  silent = TRUE
)

# fit <- ctStanFit(dat = df, ctstanmodel = mod_DTBAR, iter = 2000, chains = 4)
# summary(fit)

mod_CTBAR <- ctModel(
  type = "ct",
  id = "id",
  time = "time",
  n.manifest = 2,
  manifestNames = c("y", "x"),
  manifesttype = c(0, 1),
  n.latent = 2,
  latentNames = c("etaY", "etaX"),
  n.TDpred = 0,
  LAMBDA = diag(2),
  MANIFESTVAR = matrix(c(0, 0, 0, "thetax"), nrow = 2),
  MANIFESTMEANS = matrix(c(0, 0), nrow = 2),
  DRIFT = c("auto"), 
  CINT = matrix(c(0, "cintx"), nrow = 2),  
  T0MEANS = c(0, 0), 
  T0VAR = c("auto"),      
  silent = TRUE
)

# fit <- ctStanFit(dat = df, ctstanmodel = mod_CTBAR, iter = 2000, chains = 4)
# summary(fit)

## Control for Time-Varying L ----

### Univariate ----
mod_DTARXL <- ctModel(
  type = "dt",
  id = "id",
  time = "time",
  n.manifest = 1,
  manifestNames = "y",
  n.latent = 1,
  latentNames = "etaY",
  n.TDpred = 2,
  TDpredNames = c("x", "l"),  
  LAMBDA = matrix(1),
  MANIFESTVAR = matrix(0),
  MANIFESTMEANS = matrix(0),
  DRIFT = matrix("A"), 
  CINT = matrix(0),  
  T0MEANS = matrix(0), 
  T0VAR = matrix(1),      
  TDPREDEFFECT = matrix(c("Mx", "Ml"), ncol = 2),
  silent = TRUE
)

# fit <- ctStanFit(dat = df, ctstanmodel = mod_DTARXL, iter = 2000, chains = 2)
# summary(fit)

mod_CTARXL <- ctModel(
  type = "ct",
  id = "id",
  time = "time",
  n.manifest = 1,
  manifestNames = "y",
  n.latent = 1,
  latentNames = "etaY",
  n.TDpred = 2,
  TDpredNames = c("x", "l"),  
  LAMBDA = matrix(1),
  MANIFESTVAR = matrix(0),
  MANIFESTMEANS = matrix(0),
  DRIFT = matrix("A"), 
  CINT = matrix(0),  
  T0MEANS = matrix(0), 
  T0VAR = matrix(1),      
  TDPREDEFFECT = matrix(c("Mx", "Ml"), ncol = 2),
  silent = TRUE
)

# fit <- ctStanFit(dat = df, ctstanmodel = mod_CTARXL, iter = 2000, chains = 2)
# summary(fit)

### Multivariate ----

mod_DTVAR <- ctModel( # 
  type = "dt",
  id = "id",
  time = "time",
  n.manifest = 3,
  manifestNames = c("y", "x", "l"),
  manifesttype = c(0, 1, 0),
  n.latent = 3,
  latentNames = c("etaY", "etaX", "etaL"),
  n.TDpred = 0,
  LAMBDA = diag(3),
  MANIFESTVAR = matrix(c(0, 0, 0, 0, "thetax", 0, 0, 0, 0), nrow = 3),
  MANIFESTMEANS = matrix(c(0, 0, 0), nrow = 3),
  DRIFT = c("auto"), 
  CINT = matrix(c(0, "cintx", 0), nrow = 3),  
  T0MEANS = c(0, 0, 0), 
  T0VAR = c("auto"),      
  silent = TRUE
)

# fit <- ctStanFit(dat = df, ctstanmodel = mod_DTVAR, iter = 2000, chains = 4)
# summary(fit)

mod_CTVAR <- ctModel(
  type = "dt",
  id = "id",
  time = "time",
  n.manifest = 3,
  manifestNames = c("y", "x", "l"),
  manifesttype = c(0, 1, 0),
  n.latent = 3,
  latentNames = c("etaY", "etaX", "etaL"),
  n.TDpred = 0,
  LAMBDA = diag(3),
  MANIFESTVAR = matrix(c(0, 0, 0, 0, "thetax", 0, 0, 0, 0), nrow = 3),
  MANIFESTMEANS = matrix(c(0, 0, 0), nrow = 3),
  DRIFT = c("auto"), 
  CINT = matrix(c(0, "cintx", 0), nrow = 3),  
  T0MEANS = c(0, 0, 0), 
  T0VAR = c("auto"),      
  silent = TRUE
)

# df_lag0 <- add_lag0_time(df, c("x","l"), 0.001)
# fit <- ctStanFit(dat = df, ctstanmodel = mod_CTVAR, iter = 2000, chains = 4)
# summary(fit)

