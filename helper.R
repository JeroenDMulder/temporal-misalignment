#' Run Single Simulation Condition
#'
#' @param cond_row Integer denoting the row number in the conditions table.
#' @param R Integer denoting number of replications.
#' @inheritParams generate.R
#' @param mods_ctsem Character vector with names of ctsem models to run.
#' @param params_ctsem Character vector with parameters from ctsem models to save.
#' @param mods_Mplus Character vector with names of Mplus models to run (in registry). 
#' @param model_registry 
#' @param mplus_templates 
#' @param base_folder
run_condition <- function(
    cond_row, 
    R,
    beta, delta, gamma,
    n_id, n_t, n_burn,
    sigma2_l,
    x_change_between,
    mods_ctsem,
    params_ctsem,
    mods_Mplus,
    model_registry,
    mplus_templates,
    base_folder, 
    model_wide = FALSE
) {
  
  # Errors
  if (model_wide & !is.null(mods_ctsem)) { 
    stop("When `model_wide == TRUE`, no ctsem models can be fitted.")
  }
  
  # Create folder
  save_folder <- file.path(
    base_folder,
    paste0("m", cond_row$m, "sub", cond_row$undersample)
  )
  dir.create(save_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Run replications
  with_progress({
    p <- progressor(steps = R)
    
    res <- future_lapply(seq_len(R), function(r) {
      
      ans <- one_replication(
        r = r,
        n_id = n_id,
        n_t = n_t,
        n_burn = n_burn,
        beta = beta,
        delta = delta,
        gamma = gamma,
        sigma2_y = cond_row$sigma2_y,
        sigma2_l = sigma2_l,
        m = cond_row$m,
        sub = cond_row$undersample,
        x_change_between = x_change_between,
        mods_ctsem = mods_ctsem,
        params = params_ctsem,
        save_folder = save_folder, 
        model_wide = model_wide
      )
      
      p()
      ans
    }, future.seed = TRUE)
  })
  
  res_ctsem <- do.call(rbind, res)
  
  # Write dfList
  cat(
    paste0("df", seq_len(R), ".dat", collapse = "\n"),
    file = file.path(save_folder, "dfList.dat")
  )
  
  # Run Mplus models
  res_Mplus <- lapply(
    mods_Mplus,
    fit_model_from_registry,
    registry = model_registry,
    templates = mplus_templates,
    save_folder = save_folder,
    m = cond_row$m,
    sub = cond_row$undersample,
    model_wide = model_wide
  )
  
  res_Mplus <- do.call(rbind, res_Mplus)
  
  # Return combined result
  rbind(res_ctsem, res_Mplus, make.row.names = FALSE)
}


#' Generate Single Dataset and Fit ctsem Models
#'
#' @param r Replication number
#' @inheritParams generate.R
#' @param m An integer denoting the number of time points to aggregate over.
#' @param sub An integer denoting the number of time points to subsample. 
#' @param Mplus A boolean denoting if data should be saved for analysis in Mplus
#' @param mods_ctsem A list of models of class `ctModel`.
#' @param params A character vector of parameters to extract from a ctsem model. 
#' @param save_folder A character string denoting the path in which to save the generated datasets. 
one_replication <- function(
    r, 
    n_id, n_t, n_burn, beta, delta, gamma, sigma2_y, sigma2_l, 
    m, sub,
    x_change_between,
    Mplus, 
    mods_ctsem,
    params,
    save_folder = NULL, 
    model_wide = FALSE
  ) {
  
  # Generate data 
  df <- simulate_ARXL(
    n_id = n_id, 
    n_t = n_t, 
    n_burn = n_burn, 
    beta = beta, 
    delta = delta, 
    gamma = gamma, 
    sigma2_y = sigma2_y, 
    sigma2_l = sigma2_l, 
    m = m, 
    undersample = sub,
    x_change_between = x_change_between
  )
  
  # Covert to wide-format data
  if (model_wide) {
    df <- tidyr::pivot_wider(
      data = df,
      id_cols = id,                     
      names_from = time,          
      values_from = c(y, x, l),
      names_sep = ""
    )
  }
  
  # Write data
  if (!is.null(save_folder)) {
    save_path <- file.path(save_folder, paste0("df", r, ".dat"))
    data.table::fwrite(df, save_path, sep = "\t", 
                       col.names = FALSE, row.names = FALSE, na = ".") 
  }  
  
  # When no ctsem models to fit
  if(is.null(mods_ctsem)) {
    return(NULL)
  }
  
  # Analyze with ctsem
  out <- lapply(seq_along(mods_ctsem), function(i) {
    
    # Select model
    model <- mods_ctsem[[i]]
    model_name <- names(mods_ctsem)[i]
    
    # Fit model
    fit <-  ctStanFit(dat = df, ctstanmodel = model, iter = 2000, chains = 2, silent = TRUE)
    
    # Extract summary
    popmeans <- summary(fit)$popmeans
      
    # Create data.frame
    do.call(
      rbind, 
      lapply(params, function(p) {
        data.frame(
          rep = r,
          model = model_name,
          m = m,
          sub = sub,
          par = p,
          est = popmeans[p, "mean"],
          se = popmeans[p, "sd"],
          lb = popmeans[p, "2.5%"],
          ub = popmeans[p, "97.5%"],
          row.names = NULL
        )
      })
    )
  })
  
  out <- do.call(rbind, out)
  return(out)
}

