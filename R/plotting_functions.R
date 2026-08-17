# Advanced R Final Project
# Week 3: Diagnostic plotting functions
#
# Purpose:
# Create diagnostic plots using the auxiliary
# lm model created in run_regression().


# Create residuals-versus-fitted plot
plot_residuals_vs_fitted <- function(diagnostic_model) {
  if (!inherits(diagnostic_model, "lm")) {
    stop(
      "The diagnostic model must be an lm object.",
      call. = FALSE
    )
  }
  plot_data <- data.frame(
    fitted = stats::fitted(diagnostic_model),
    residuals = stats::residuals(diagnostic_model)
  )
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = fitted,
      y = residuals
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::labs(
      title = "Residuals vs Fitted",
      x = "Fitted Values",
      y = "Residuals"
    ) +
    ggplot2::theme_minimal()
  return(plot)
}

# Create normal QQ plot

plot_qq <- function(diagnostic_model) {
  
  if (!inherits(diagnostic_model, "lm")) {
    
    stop(
      
      "The diagnostic model must be an lm object.",
      
      call. = FALSE
      
    )
    
  }
  
  plot_data <- data.frame(
    
    residuals = stats::residuals(diagnostic_model)
    
  )
  
  plot <- ggplot2::ggplot(
    
    plot_data,
    
    ggplot2::aes(sample = residuals)
    
  ) +
    
    ggplot2::stat_qq() +
    
    ggplot2::stat_qq_line() +
    
    ggplot2::labs(
      
      title = "Normal QQ Plot",
      
      x = "Theoretical Quantiles",
      
      y = "Sample Quantiles"
      
    ) +
    
    ggplot2::theme_minimal()
  
  return(plot)
  
}
