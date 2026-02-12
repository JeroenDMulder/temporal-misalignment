compute_performance <- function(
    data,
    R
) {
  
  summ <- data |>
    dplyr::group_by(m, sub, model, par) |>
    dplyr::summarise(
      avg = mean(est),
      MCSE_avg = sqrt((1 / (R * (R - 1))) * sum( (est - avg)^2) ),
      bias = avg - dplyr::first(pop_value),
      MCSE_bias = sqrt((1 / (R * (R - 1))) * sum( (est - avg)^2) ),
      SD = sd(est),
      mse = (1 / R) * sum(bias^2),
      MCSE_mse = sqrt((sum((bias^2) - mse)^2) / (R * (R - 1))),
      coverage = mean(cover),
      MCSE_coverage = sqrt((coverage * (1 - coverage)) / R),
      rejectionH0 = mean(sig),
      MCSE_rejectionH0 = sqrt((rejectionH0 * (1 - rejectionH0)) / R),
      .groups = "drop"
    )
  
  summ |>
    tidyr::pivot_longer(
      cols = c(avg, bias, SD, mse, coverage, rejectionH0),
      names_to = "metric",
      values_to = "value"
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("MCSE_"),
      names_to = "metric_mcse",
      values_to = "MCSE"
    ) |>
    dplyr::filter(
      metric == sub("MCSE_", "", metric_mcse) |
        (metric %in% c("SD") & metric_mcse == "MCSE_bias")
    ) |>
    dplyr::mutate(
      MCSE = dplyr::if_else(metric %in% c("SD"), NA_real_, MCSE)
    ) |>
    dplyr::select(-metric_mcse) |>
    dplyr::mutate(
      m = factor(m, levels = sort(unique(m)), ordered = TRUE)
    )
}
