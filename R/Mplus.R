# Mplus Input File ----
fit_model_from_registry <- function(
    model_name,
    registry,
    templates,
    save_folder,
    m,
    sub, 
    model_wide
) { 
  spec <- registry[[model_name]]
  
  # Write Mplus input
  path <- write_Mplus_generic(
    template = templates[[spec$template]],
    placeholders = spec$placeholders(
      m = m,
      sub = sub
    ),
    save_folder = save_folder,
    inp_name = spec$inp_name(m, sub)
  )
  
  # Run Mplus
  res <- run_Mplus(
    path = path,
    m = m,
    sub = sub,
    params = spec$params_saved,
    name_model = model_name,
    model_wide = model_wide
  )
  
  # Filter parameters
  res[res$par %in% spec$params_select, ]
}

write_Mplus_generic <- function(
    template,
    placeholders,
    save_folder,
    inp_name
) { 
  synt <- template
  
  for (nm in names(placeholders)) {
    synt <- gsub(nm, placeholders[[nm]], synt, fixed = TRUE)
  }
  
  file_path <- file.path(save_folder, inp_name)
  cat(synt, file = file_path)
  file_path
}


mplus_templates <- list(
  
  DTARLag1X = "TITLE:
  Project - Mulder, Voelkle, and Hamaker (in preparation)
  Model used to determine the total effect of lagged X on Y

DATA:
  FILE = dfList.dat;
  TYPE = MONTECARLO;

VARIABLE:
  NAMES = id time Y X L;
  USEVARIABLES = Y X;
  LAGGED = X(1) Y(1);

ANALYSIS:
  ESTIMATOR = BAYES;
  PROCESSOR = 4;
  BITERATION = (2000);

MODEL:
  Y ON Y&1 X&1 (b_YLag1Y b_YLag1X);
  
MODEL CONSTRAINT: 
  NEW(totLag1X);
  totLag1X = (0.3 * b_YLag1Y) + b_YLag1X; 

SAVEDATA:
  RESULTS = _PLACEHOLDEROUT_.dat;",
  
  DTLag1X = "TITLE:
  Project - Mulder, Voelkle, and Hamaker (in preparation)
  Model - Estimate total lag-1 effect of X on Y

DATA:
  FILE = dfList.dat;
  TYPE = MONTECARLO;

VARIABLE:
  NAMES = id time Y X L;
  USEVARIABLES = Y X;
  LAGGED = X(1) Y(2);

ANALYSIS:
  ESTIMATOR = BAYES;
  PROCESSOR = 4;
  BITERATION = (2000);

MODEL:
  Y ON X&1 Y&2;
  X ON Y&1; 
  Y WITH X@0;

SAVEDATA:
  RESULTS = _PLACEHOLDEROUT_.dat;",
  
  DTLag1XMulti = "TITLE:
  Project - Mulder, Voelkle, and Hamaker (in preparation)
  Model - Estimate total lag-1 effect of X on Y, including autoregression for X

DATA:
  FILE = dfList.dat;
  TYPE = MONTECARLO;

VARIABLE:
  NAMES = id time Y X L;
  USEVARIABLES = Y X;
  LAGGED = X(1) Y(2);

ANALYSIS:
  ESTIMATOR = BAYES;
  PROCESSOR = 4;
  BITERATION = (2000);

MODEL:
  Y ON X&1 Y&2;
  X ON X&1 Y&1; 
  Y WITH X@0;

SAVEDATA:
  RESULTS = _PLACEHOLDEROUT_.dat;",
  
  DTLag1ExoX = "TITLE:
  Project - Mulder, Voelkle, and Hamaker (in preparation)
  Model - Estimate total lag-1 effect of X on Y when X is exogenous

DATA:
  FILE = dfList.dat;
  TYPE = MONTECARLO;

VARIABLE:
  NAMES = id time Y X L;
  USEVARIABLES = Y X;
  LAGGED = X(1);

ANALYSIS:
  ESTIMATOR = BAYES;
  PROCESSOR = 4;
  BITERATION = (2000);

MODEL:
  Y ON X&1;

SAVEDATA:
  RESULTS = _PLACEHOLDEROUT_.dat;",
  
  DTBAR = "TITLE:
  Project - Mulder, Voelkle, and Hamaker (in preparation)
  Model - DT BAR(1) (i.e., traditional CLPM)

DATA:
  FILE = dfList.dat;
  TYPE = MONTECARLO;

VARIABLE:
  NAMES = id time Y X L;
  USEVARIABLES = Y X;
  LAGGED = Y(1) X(1);

ANALYSIS:
  ESTIMATOR = BAYES;
  PROCESSOR = 4;
  BITERATION = (2000);

MODEL:
  Y ON Y&1 X&1;
  X ON Y&1 X&1;
  Y WITH X;

SAVEDATA:
  RESULTS = _PLACEHOLDEROUT_.dat;",
  
  DTSBAR = "TITLE:
  Project - Mulder, Voelkle, and Hamaker (in preparation)
  Model - DT BAR(1) with lag-0 effect of X

DATA:
  FILE = dfList.dat;
  TYPE = MONTECARLO;

VARIABLE:
  NAMES = id time Y X L;
  USEVARIABLES = Y X;
  LAGGED = Y(1) X(1);

ANALYSIS:
  ESTIMATOR = BAYES;
  PROCESSOR = 4;
  BITERATION = (2000);

MODEL:
  Y ON X Y&1 X&1;
  X ON Y&1 X&1;

SAVEDATA:
  RESULTS = _PLACEHOLDEROUT_.dat;", 
  
  DTSVAR = "TITLE:
  Project - Mulder, Voelkle, and Hamaker (in preparation)
  Model - DT VAR(1)Lag0

DATA:
  FILE = dfList.dat;
  TYPE = MONTECARLO;

VARIABLE:
  NAMES = id time Y X L;
  USEVARIABLES = Y X L;
  LAGGED = Y(1) X(1) L(1);

ANALYSIS:
  ESTIMATOR = BAYES;
  PROCESSOR = 4;
  BITERATION = (2000);

MODEL:
  Y ON X L Y&1 X&1 L&1; 
  X ON L Y&1 X&1 L&1;
  L ON Y&1 X&1 L&1;
  
SAVEDATA:
  RESULTS = _PLACEHOLDEROUT_.dat;",
  
  DTLag1XL = "TITLE:
  Project - Mulder, Voelkle, and Hamaker (in preparation)
  Model - Estimate univariately total lag-1 effect of X on Y controling for L

DATA:
  FILE = dfList.dat;
  TYPE = MONTECARLO;

VARIABLE:
  NAMES = id time Y X L;
  USEVARIABLES = Y X L;
  LAGGED = X(1) Y(2) L(1);

ANALYSIS:
  ESTIMATOR = BAYES;
  PROCESSOR = 4;
  BITERATION = (2000);

MODEL:
  Y ON X&1 Y&2 L&1;
  X ON Y&1; 
  Y WITH X@0 L@0;

SAVEDATA:
  RESULTS = _PLACEHOLDEROUT_.dat;",
  
  DTLag1XLMulti = "TITLE:
  Project - Mulder, Voelkle, and Hamaker (in preparation)
  Model - Estimate total lag-1 effect of X on Y controling for L

DATA:
  FILE = dfList.dat;
  TYPE = MONTECARLO;

VARIABLE:
  NAMES = id time Y X L;
  USEVARIABLES = Y X L;
  LAGGED = X(1) Y(2) L(1);

ANALYSIS:
  ESTIMATOR = BAYES;
  PROCESSOR = 4;
  BITERATION = (2000);

MODEL:
  Y ON X&1 Y&2 L&1;
  X ON Y&1 L&1 X&1; 
  L ON Y&1 X&1 L&1; 
  Y WITH X@0 L@0;

SAVEDATA:
  RESULTS = _PLACEHOLDEROUT_.dat;",
  
  DTCLPM = "TITLE:
  Project - Mulder, Voelkle, and Hamaker (in preparation)
  Model - CLPM with marginal lag-1 effects (ommision of lag-1 Y)

DATA:
  FILE = dfList.dat;
  TYPE = MONTECARLO;

VARIABLE:
  NAMES = id y1-y5 x1-x5 l1-l5;
  USEVARIABLES = Y1-Y5 X1-X5 L1-L5;

MODEL:
  x2-x5 y3-y5 l2-l5 PON x1-x4 y1-y3 l1-l4; ! Autoregressive effects (with lag-2 effects for Y)
  
  x2-x5 y2-y5 l2-l5 PON l1-l4 x1-x4 y1-y4; ! Cross-lagged effects 
  x2-x5 y2-y5 l2-l5 PON y1-y4 l1-l4 x1-x4; 
  
  ! Residual covariances
  x1-x5 PWITH y1-y5@0;
  x1-x5 PWITH l1-l5;
  y1-y5 PWITH l1-l5;

SAVEDATA:
 RESULTS = _PLACEHOLDEROUT_.dat;",
  
  DTCLPMLag0 = "TITLE:
  Project - Mulder, Voelkle, and Hamaker (in preparation)
  Model - CLPM with lag-0 effects

DATA:
  FILE = dfList.dat;
  TYPE = MONTECARLO;

VARIABLE:
  NAMES = id y1-y5 x1-x5 l1-l5;
  USEVARIABLES = Y1-Y5 X1-X5 L1-L5;

MODEL:
  x2-x5 y2-y5 l2-l5 PON x1-x4 y1-y4 l1-l4 (b1-b12); ! Autoregressive effects
  
  x2-x5 y2-y5 l2-l5 PON l1-l4 x1-x4 y1-y4 (c1-c12); ! Cross-lagged effects 
  x2-x5 y2-y5 l2-l5 PON y1-y4 l1-l4 x1-x4 (d1-d12); 
  
  ! Lag-0 effects
  y1-y5 PON x1-x5 (e1-e5);
  y1-y5 PON l1-l5;
  x1-x5 PON l1-l5;
  
SAVEDATA:
 RESULTS = _PLACEHOLDEROUT_.dat;"
)


model_registry <- list(
  
  DTARLag1X = list(
    template = "DTARLag1X",
    
    placeholders = function(m, sub) {
      list("_PLACEHOLDEROUT_" = paste0("res_DTARLag1X_m", m, "_sub", sub))
    },
    
    inp_name = function(m, sub) {
      paste0("DTARLag1X_m", m, "_sub", sub, ".inp")
    },
    
    params_saved  = c("NU_X", "THETA_X",
                      "ALPHA_Y", 
                      "BETA_YLag1Y", "BETA_YLag1X",  
                      "PSI_Y", "BETA_TotLag1X"),
    params_select = "BETA_TotLag1X"
  ),
  
  DTLag1X = list(
    template = "DTLag1X",
    
    placeholders = function(m, sub) {
      list("_PLACEHOLDEROUT_" = paste0("res_DTLag1X_m", m, "_sub", sub))
    },
    
    inp_name = function(m, sub) {
      paste0("DTLag1X_m", m, "_sub", sub, ".inp")
    },
    
    params_saved  = c("ALPHA_Y", "ALPHA_X",  
                      "BETA_YLag1X", "BETA_YLag2Y", "BETA_XLag1Y",
                      "PSI_Y", "PSI_X"),
    params_select = "BETA_YLag1X"
  ),
  
  DTLag1XMulti = list(
    template = "DTLag1XMulti",
    
    placeholders = function(m, sub) {
      list("_PLACEHOLDEROUT_" = paste0("res_DTLag1XMulti_m", m, "_sub", sub))
    },
    
    inp_name = function(m, sub) {
      paste0("DTLag1XMulti_m", m, "_sub", sub, ".inp")
    },
    
    params_saved  = c("ALPHA_Y", "ALPHA_X",  
                      "BETA_YLag1X", "BETA_YLag2Y", 
                      "BETA_XLag1X", "BETA_XLag1Y",
                      "PSI_Y", "PSI_X"),
    params_select = "BETA_YLag1X"
  ),
  
  DTLag1ExoX = list(
    template = "DTLag1ExoX",
    
    placeholders = function(m, sub) {
      list("_PLACEHOLDEROUT_" = paste0("res_DTLag1ExoX_m", m, "_sub", sub))
    },
    
    inp_name = function(m, sub) {
      paste0("DTLag1X_m", m, "_sub", sub, ".inp")
    },
    
    params_saved  = c("ALPHA_Y", "ALPHA_X",  
                      "BETA_YLag1X", "BETA_YLag2Y", "BETA_XLag1Y",
                      "PSI_Y", "PSI_X"),
    params_select = "BETA_YLag1X"
  ),
  
  DTBAR = list(
    template = "DTBAR",
    
    placeholders = function(m, sub) {
      list(
        "_PLACEHOLDEROUT_" = paste0("res_DTBAR_m", m, "_sub", sub)
      )
    },
    
    inp_name = function(m, sub) {
      paste0("DTBAR_m", m, "_sub", sub, ".inp")
    },
    
    params_saved  = c("ALPHA_Y", "ALPHA_X",
                      "BETA_YLag1Y", "BETA_YLag1X", "BETA_XLag1Y", "BETA_XLag1X",
                       "PSI_Y", "PSI_XY", "PSI_X"),
    params_select = "BETA_YLag1X"
  ),
  
  DTSBAR = list(
    template = "DTSBAR",
    
    placeholders = function(m, sub) {
      list(
        "_PLACEHOLDEROUT_" = paste0("res_DTSBAR_m", m, "_sub", sub)
      )
    },
    
    inp_name = function(m, sub) {
      paste0("DTSBAR_m", m, "_sub", sub, ".inp")
    },
    
    params_saved = c("ALPHA_Y", "ALPHA_X", 
                     "BETA_YX", "BETA_YLag1Y", "BETA_YLag1X", 
                     "BETA_XLag1Y", "BETA_XLag1X", 
                     "PSI_Y", "PSI_X"),
    params_select = "BETA_YX"
  ),
  
  DTSVAR = list(
    template = "DTSVAR", 
    
    placeholders = function(m, sub) {
      list(
        "_PLACEHOLDEROUT_" = paste0("res_DTSVAR_m", m, "_sub", sub)
      )
    },
    
    inp_name = function(m, sub) {
      paste0("DTSVAR_m", m, "_sub", sub, ".inp")
    },
    
    params_saved = c("ALPHA_Y", "ALPHA_X", "ALPHA_L", 
                     "BETA_YX", "BETA_YL", "BETA_YLag1Y", "BETA_YLag1X", "BETA_YLag1L", 
                     "BETA_XL", "BETA_XLag1Y", "BETA_XLag1X", "BETA_XLag1L",
                     "BETA_LLag1Y", "BETA_LLag1X", "BETA_LLag1L", 
                     "PSI_Y", "PSI_X", "PSI_L"),
    params_select = c("BETA_YLag1Y", "BETA_YLag1X", "BETA_YX")
  ), 
  
  DTLag1XL = list(
    template = "DTLag1XL",
    
    placeholders = function(m, sub) {
      list("_PLACEHOLDEROUT_" = paste0("res_DTLag1XL_m", m, "_sub", sub))
    }, 
    
    inp_name = function(m, sub) {
      paste0("DTLag1XL_m", m, "_sub", sub, ".inp")
    },
    
    params_saved = c("ALPHA_Y", "ALPHA_X", "ALPHA_L",  
                     "BETA_YLag1X", "BETA_YLag2Y", "BETA_YLag1L", "BETA_XLag1Y",
                     "PSI_Y", "PSI_X", "PSI_L"),
    params_select = c("BETA_YLag1X")
  ), 
  
  DTLag1XLMulti = list(
    template = "DTLag1XLMulti",
    
    placeholders = function(m, sub) {
      list("_PLACEHOLDEROUT_" = paste0("res_DTLag1XLMulti_m", m, "_sub", sub))
    }, 
    
    inp_name = function(m, sub) {
      paste0("DTLag1XLMulti_m", m, "_sub", sub, ".inp")
    },
    
    params_saved = c("ALPHA_Y", "ALPHA_X", "ALPHA_L", 
                     "BETA_YLag1X", "BETA_YLag2Y", "BETA_YLag1L", 
                     "BETA_XLag1X", "BETA_XLag1Y", "BETA_XLag1L",
                     "BETA_LLag1X", "BETA_LLag1Y", "BETA_LLag1L",
                     "PSI_Y", "PSI_X", "PSI_LX", "PSI_L"),
    params_select = c("BETA_YLag1X")
  ),
  
  DTCLPM = list(
    template = "DTCLPM",
    
    placeholders = function(m, sub) {
      list("_PLACEHOLDEROUT_" = paste0("res_DTCLPM_m", m, "_sub", sub))
    }, 
    
    inp_name = function(m, sub) {
      paste0("DTCLPM_m", m, "_sub", sub, ".inp")
    },
    
    params_saved = c("ALPHA_Y2", "ALPHA_Y3", "ALPHA_Y4", "ALPHA_Y5", 
                     "ALPHA_X2", "ALPHA_X3", "ALPHA_X4", "ALPHA_X5", 
                     "ALPHA_L2", "ALPHA_L3", "ALPHA_L4", "ALPHA_L5",
                     "ALPHA_Y1", "ALPHA_X1", "ALPHA_L1",
                     
                     "BETA_Y2X1", "BETA_Y2L1", 
                     "BETA_Y3X2", "BETA_Y3L2", "BETA_Y3Y1", 
                     "BETA_Y4Y2", "BETA_Y4X3", "BETA_Y4L3", 
                     "BETA_Y5Y3", "BETA_Y5X4", "BETA_Y5L4", 
                     
                     "BETA_X2Y1", "BETA_X2X1", "BETA_X2L1",
                     "BETA_X3Y2", "BETA_X3X2", "BETA_X3L2",
                     "BETA_X4Y3", "BETA_X4X3", "BETA_X4L3",
                     "BETA_X5Y4", "BETA_X5X4", "BETA_X5L4",
                     
                     "BETA_L2Y1", "BETA_L2X1", "BETA_L2L1",
                     "BETA_L3Y2", "BETA_L3X2", "BETA_L3L2",
                     "BETA_L4Y3", "BETA_L4X3", "BETA_L4L3",
                     "BETA_L5Y4", "BETA_L5X4", "BETA_L5L4",
                     
                     "PSI_Y2", "PSI_Y3", "PSI_Y4", "PSI_Y5",
                     "PSI_X2", "PSI_X3", "PSI_X4", "PSI_X5",
                     
                     "PSI_L2Y2", "PSI_L2X2", "PSI_L2", 
                     "PSI_L3Y3", "PSI_L3X3", "PSI_L3",
                     "PSI_L4Y4", "PSI_L4X4", "PSI_L4",
                     "PSI_L5Y5", "PSI_L5X5", "PSI_L5",
                     
                     "PSI_Y1", "PSI_X1", 
                     "PSI_L1Y1", "PSI_L1X1", "PSI_L1"),
    params_select = c("BETA_Y5X4", "BETA_Y4X3", "BETA_Y3X2", "BETA_Y2X1")
  ), 
  
  DTCLPMLag0 = list(
    template = "DTCLPMLag0",
    
    placeholders = function(m, sub) {
      list("_PLACEHOLDEROUT_" = paste0("res_DTCLPMLag0_m", m, "_sub", sub))
    }, 
    
    inp_name = function(m, sub) {
      paste0("DTCLPMLag0_m", m, "_sub", sub, ".inp")
    },
    
    params_saved = c("ALPHA_Y1", "ALPHA_Y2", "ALPHA_Y3", "ALPHA_Y4", "ALPHA_Y5", 
                     "ALPHA_X1", "ALPHA_X2", "ALPHA_X3", "ALPHA_X4", "ALPHA_X5", 
                     "ALPHA_L2", "ALPHA_L3", "ALPHA_L4", "ALPHA_L5",  
                     
                     "BETA_Y1X1", "BETA_Y1L1", 
                     "BETA_Y2Y1", "BETA_Y2X1", "BETA_Y2X2", "BETA_Y2L2", "BETA_Y2L1", 
                     "BETA_Y3Y2", "BETA_Y3X2", "BETA_Y3X3", "BETA_Y3L2", "BETA_Y3L3",
                     "BETA_Y4Y3", "BETA_Y4X3", "BETA_Y4X4", "BETA_Y4L3", "BETA_Y4L4", 
                     "BETA_Y5Y4", "BETA_Y5X4", "BETA_Y5X5", "BETA_Y5L4", "BETA_Y5L5", 
                     
                     "BETA_X1L1", # 37
                     "BETA_X2Y1", "BETA_X2X1", "BETA_X2L2", "BETA_X2L1", 
                     "BETA_X3Y2", "BETA_X3X2", "BETA_X3L2", "BETA_X3L3",
                     "BETA_X4Y3", "BETA_X4X3", "BETA_X4L3", "BETA_X4L4",
                     "BETA_X5Y4", "BETA_X5X4", "BETA_X5L4", "BETA_X5L5", 
                     
                     "BETA_L2Y1", "BETA_L2X1", "BETA_L2L1", # 54, 55
                     "BETA_L3Y2", "BETA_L3X2", "BETA_L3L2", 
                     "BETA_L4Y3", "BETA_L4X3", "BETA_L4L3",
                     "BETA_L5Y4", "BETA_L5X4", "BETA_L5L4", 
                     
                     "PSI_Y1", "PSI_Y2", "PSI_Y3", "PSI_Y4", "PSI_Y5", # 66, 67, ...
                     "PSI_X1", "PSI_X2", "PSI_X3", "PSI_X4", "PSI_X5",
                     "PSI_L2", "PSI_L3", "PSI_L4", "PSI_L5"),
    params_select = c("BETA_Y5X5", "BETA_Y4X4", "BETA_Y3X3", "BETA_Y2X2")
  )
)

# Estimation ----
run_Mplus <- function(path, m, sub, params, name_model, model_wide) {
  
  # Run analysis 
  MplusAutomation::runModels(target = path, quiet = FALSE)
  
  # Create path to results
  path_out <- sub("DT", "res_DT", path)
  path_out <- sub(".inp", ".dat", path_out)
  
  # Extract results
  lines <- readLines(path_out)
  
  # Collapse wrapped lines until all replications are read
  p <- length(params)
  i <- 1
  out <- list()
  
  while (i <= length(lines)) {
    
    # Replication number
    repno <- as.integer(lines[i])
    i <- i + 1
    
    # Parameter estimates
    est <- read_Mplus_MCdat(lines, i, p)
    estimates <- est$values
    i <- est$next_line
    
    # Standard errors
    se <- read_Mplus_MCdat(lines, i, p)
    ses <- se$values
    i <- se$next_line
    
    # Skip last line (free params, DIC, pD)
    if (model_wide) {
      i <- i + 2 
    } else {
      i <- i + 1
    }
    
    
    # Store results
    out[[length(out) + 1]] <- data.frame(
      rep = repno,
      model = name_model,
      m = m,
      sub = sub,
      par = params,
      est = estimates,
      se = ses,
      lb = NA, 
      ub = NA
    )
  }
  
  results_long <- do.call(rbind, out)
  return(results_long)
}

read_Mplus_MCdat <- function(lines, start, n) {
  vals <- numeric(0)
  i <- start
  while(length(vals) < n) {
    vals <- c(vals, scan(text = lines[i], quiet = TRUE))
    i <- i + 1
  }
  list(values = vals[1:n], next_line = i)
}


