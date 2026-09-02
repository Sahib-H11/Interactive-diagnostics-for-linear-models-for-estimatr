# Advanced R Final Project
# Week 3: Diagnostic functions
#
# Purpose:
# Perform formal regression diagnostics using
# the auxiliary lm model created in run_regression().





# Calculate the studentized Breusch-Pagan test
calculate_bp_test <- function(diagnostic_model) {
  
  if (!inherits(diagnostic_model, "lm")) {
    stop(
      "The diagnostic model must be an lm object.",
      call. = FALSE
    )
  }
  
  bp_test <- lmtest::bptest(
    diagnostic_model,
    studentize = TRUE
  )
  
  result <- list(
    statistic = unname(bp_test$statistic),
    degrees_of_freedom = unname(bp_test$parameter),
    p_value = bp_test$p.value,
    method = bp_test$method
  )
  
  return(result)
}

# Calculate variance inflation factors
calculate_vif <- function(diagnostic_model) {
  if (!inherits(diagnostic_model, "lm")) {
    stop(
      "The diagnostic model must be an lm object.",
      call. = FALSE
    )
  }
  # Extract predictor names from the diagnostic model
  predictor_names <- attr(
    stats::terms(diagnostic_model),
    "term.labels"
  )
  if (length(predictor_names) < 2L) {
    return(
      list(
        values = NULL,
        message = paste(
          "VIF was not calculated because the model",
          "contains only one predictor."
        )
      )
    )
  }
  # Calculate VIF values for models with at least two predictors
  vif_values <- car::vif(diagnostic_model)
  # Convert the named VIF values into a data frame for later output
  result <- list(
    values = data.frame(
      variable = names(vif_values),
      vif = as.numeric(vif_values),
      row.names = NULL
    ),
    message = NULL
  )
  return(result)
}