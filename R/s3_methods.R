# Advanced R Final Project
# Week 4: S3 methods
#
# Purpose:
# Create a custom regression_result object
# and define print() and summary() methods.
# Create a structured regression result object
create_regression_result <- function(model_result) {
  
  if (!is.list(model_result)) {
    stop(
      "The model result must be a list.",
      call. = FALSE
    )
  }
  
  required_components <- c(
    "robust_model",
    "diagnostic_model",
    "formula",
    "se_type",
    "removed_observations",
    "original_observations",
    "complete_observations"
  )
  
  missing_components <- setdiff(
    required_components,
    names(model_result)
  )
  
  if (length(missing_components) > 0L) {
    stop(
      paste(
        "The model result is missing:",
        paste(
          missing_components,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  coefficient_table <- create_coefficient_table(
    model_result$robust_model
  )
  
  bp_result <- calculate_bp_test(
    model_result$diagnostic_model
  )
  
  vif_result <- calculate_vif(
    model_result$diagnostic_model
  )
  
  residual_plot <- plot_residuals_vs_fitted(
    model_result$diagnostic_model
  )
  
  qq_plot <- plot_qq(
    model_result$diagnostic_model
  )
  
  # Start with an empty vector for user-facing warning messages.
  # Currently, this is used when VIF cannot be calculated for a single-predictor model.
  warning_messages <- character(0)
  
  if (!is.null(vif_result$message)) {
    warning_messages <- c(
      warning_messages,
      vif_result$message
    )
  }
  
  result <- list(
    robust_model = model_result$robust_model,
    diagnostic_model = model_result$diagnostic_model,
    coefficients = coefficient_table,
    diagnostics = list(
      bp_test = bp_result,
      vif = vif_result
    ),
    plots = list(
      residuals_vs_fitted = residual_plot,
      qq = qq_plot
    ),
    warnings = warning_messages,
    data_info = list(
      original_observations =
        model_result$original_observations,
      complete_observations =
        model_result$complete_observations,
      removed_observations =
        model_result$removed_observations
    ),
    settings = list(
      formula = model_result$formula,
      se_type = model_result$se_type
    )
  )
  
  # Assign the custom S3 class so print() and summary() use the methods below
  class(result) <- "regression_result"
  
  return(result)
}

# Print a concise summary of a regression_result object
print.regression_result <- function(x, ...) {
  
  cat("Regression Result\n")
  cat("-----------------\n")
  
  cat(
    "Formula:",
    paste(
      deparse(x$settings$formula),
      collapse = " "
    ),
    "\n"
  )
  
  cat(
    "Standard errors:",
    x$settings$se_type,
    "\n"
  )
  
  cat(
    "Observations:",
    x$data_info$complete_observations,
    "\n"
  )
  
  cat(
    "Removed observations:",
    x$data_info$removed_observations,
    "\n"
  )
  
  invisible(x)
}

# Print a detailed summary of a regression_result object
summary.regression_result <- function(object, ...) {
  
  model_stats <- broom::glance(
    object$robust_model
  )
  
  cat("Regression Result Summary\n")
  cat("-------------------------\n")
  
  cat(
    "Formula:",
    paste(
      deparse(object$settings$formula),
      collapse = " "
    ),
    "\n"
  )
  
  cat(
    "Standard errors:",
    object$settings$se_type,
    "\n"
  )
  
  cat(
    "Observations:",
    object$data_info$complete_observations,
    "\n"
  )
  
  cat(
    "Removed observations:",
    object$data_info$removed_observations,
    "\n"
  )
  
  cat(
    "R-squared:",
    round(model_stats$r.squared, 3),
    "\n"
  )
  
  cat(
    "Adjusted R-squared:",
    round(model_stats$adj.r.squared, 3),
    "\n"
  )
  
  cat("\nStudentized Breusch-Pagan Test\n")
  
  cat(
    "Statistic:",
    round(
      object$diagnostics$bp_test$statistic,
      3
    ),
    "\n"
  )
  
  cat(
    "p-value:",
    round(
      object$diagnostics$bp_test$p_value,
      4
    ),
    "\n"
  )
  
  cat("\nVariance Inflation Factors\n")
  
  # Falls back to the explanatory message when VIF was not calculated
  # for a single-predictor model.
  if (is.null(object$diagnostics$vif$values)) {
    
    cat(
      object$diagnostics$vif$message,
      "\n"
    )
    
  } else {
    
    print(
      object$diagnostics$vif$values,
      row.names = FALSE
    )
    
  }
  
  invisible(object)
}
