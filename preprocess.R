preprocess1_results <- function(
    results,
    conds,
    beta,
    par_keep,
    remove_lag0_models,
    compute_ci_models, 
    truth_lag1k
) {
  
  if (!(all(compute_ci_models %in% unique(results$model)))) {
    stop("Some models in `compute_ci_models` are not in `results`.")
  }
  
  results |>
    
    # Filter out autoregressive effects (not of primary interest)
    dplyr::filter(par %in% par_keep) |>
    
    # Add true lag1k values
    left_join(select(truth_lag1k, m, sub, true_lag1k), by = c("m", "sub")) |> 
    
    dplyr::mutate(
      # Fill in missing lb and ub based on normal approximation
      lb = dplyr::case_when(
        model %in% compute_ci_models ~ est - 1.96 * se,
        TRUE ~ lb
      ),
      ub = dplyr::case_when(
        model %in% compute_ci_models ~ est + 1.96 * se,
        TRUE ~ ub
      ),
      
      # Recode parameter names
      par = dplyr::case_when(
        par == "Mx" ~ "lag0",
        par == "BETA_YX" ~ "lag0",
        par == "BETA_YLag1X" ~ "lag1k",
        TRUE ~ par
      ),
      
      # Assign population values
      pop_value = dplyr::case_when(
        par == "lag0" ~ beta[2],
        par == "lag1k" ~ true_lag1k,
        TRUE ~ NA_real_
      ),
      
      # Coverage and significance
      cover = lb < pop_value & ub > pop_value,
      sig   = lb > 0 | ub < 0
    ) |>
    
    # Filter out theoretically impossible lag-0 scenarios
    dplyr::filter(!(model %in% remove_lag0_models & m > 1)) |> 
    
    # Remove helper columns
    dplyr::select(-true_lag1k)
}


extract_simulated_true_lag1k <- function(dat) {
  
  if (!("DTARLag1X" %in% dat$model)) {
    stop("The model DTARLag1X is not in the results.")
  }
  
  out <- dat |> 
    dplyr::filter(model == "DTARLag1X" & m == 1) |> 
    dplyr::group_by(m, sub) |>
    dplyr::summarise(true_lag1k = mean(est), .groups = "drop") |> 
    dplyr::mutate(
      facet_col_fct = as.factor(m), 
      model_lab = "Simulated Truth", 
      metric = "avg" 
    ) 
  return(out)
}
