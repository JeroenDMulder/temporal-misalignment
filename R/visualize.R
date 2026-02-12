make_par_plot <- function(
    data,
    par_select,
    x_var,
    facet_col_var,
    x_lab = NULL,
    model_select,
    scenario_select, 
    model_color,
    metric_labs,
    y_scales,
    hlines,
    save_path = NULL,
    width = NA,
    height = NA, 
    truth = NULL
) {

  stopifnot(
    is.character(x_var),
    is.character(facet_col_var),
    length(x_var) == 1,
    length(facet_col_var) == 1
  )
  
  metrics_to_plot <- names(metric_labs)
  
  # Get relevant data
  plot_data <- data |>
    dplyr::filter(
      par %in% par_select,
      model %in% model_select,
      metric %in% metrics_to_plot, 
      scenario %in% scenario_select,
    ) |>
    dplyr::mutate(
      metric = factor(metric, levels = metrics_to_plot),
      x_var_fct = forcats::as_factor(.data[[x_var]]),
      facet_col_fct = forcats::as_factor(.data[[facet_col_var]]), 
      `True Value` = ifelse(grepl("Null", scenario), "Null", "Non-null")
    ) 
  
  # Create the plot
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = x_var_fct,
      y = value,
      color = as.factor(`True Value`),
      shape = as.factor(model_type),
      group = interaction(`True Value`, model_type), 
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = value - MCSE * 1.96,
        ymax = value + MCSE * 1.96
      ),
      width = 0.3
    ) +
    ggplot2::geom_hline(
      data = hlines |>
        dplyr::filter(metric %in% metrics_to_plot) |>
        dplyr::mutate(metric = factor(metric, levels = metrics_to_plot)),
      ggplot2::aes(yintercept = yintercept),
      color = "black",
      linewidth = 0.6,
      linetype = "dotted"
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(metric),
      cols = ggplot2::vars(facet_col_fct),
      switch = "y",
      scales = "free_y",
      labeller = ggplot2::labeller(metric = metric_labs)
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      strip.placement = "outside",
      strip.background = ggplot2::element_rect(
        fill = "grey90",
        color = "grey50",
        linewidth = 0.5
      ),
      panel.grid.major = ggplot2::element_line(
        color = "grey85",
        linewidth = 0.3
      ),
      panel.grid.minor = ggplot2::element_blank(), 
      text = ggplot2::element_text(size = 12)
    ) +
    ggplot2::labs(
      x = x_lab,
      y = NULL
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(title = "Lag-1 Effect:"), 
      shape = ggplot2::guide_legend(title = "Model:")
    )
  
  # Optional: Add population-level effect
  if(!is.null(truth)) {
    p <- p + 
      geom_point(
        mapping = aes(
          x = as.factor(sub), 
          y = true_lag1k
        ), 
        data = truth, 
        shape = 4, 
        size = 4
      )
  }
  
  # Optional: Set y-axis limits
  if (!is.null(y_scales) && !all(is.na(y_scales))) {
    p <- p + ggh4x::facetted_pos_scales(
      y = y_scales[metrics_to_plot]
    )
  }
  
  # Optional: Save
  if (!is.null(save_path)) {
    ggplot2::ggsave(
      filename = save_path,
      plot = p,
      width = width,
      height = height
    )
  }
  
  return(p)
}
