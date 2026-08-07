# Advanced R Final Project
# Week 2: Model estimation functions
#
# Purpose:
# Create the model formula and estimate both the main
# robust model and the auxiliary diagnostic model.


# Run the regression using the prepared model data
run_regression <- function(
    data,
    dependent_var,
    independent_vars,
    se_type = "HC3"
) {
  
  prepared_data <- prepare_model_data(
    data = data,
    dependent_var = dependent_var,
    independent_vars = independent_vars
  )
  
  model_formula <- stats::reformulate(
    termlabels = independent_vars,
    response = dependent_var
  )
  
  robust_model <- estimatr::lm_robust(
    formula = model_formula,
    data = prepared_data$data,
    se_type = se_type
  )
  
  diagnostic_model <- stats::lm(
    formula = model_formula,
    data = prepared_data$data
  )
  
  result <- list(
    robust_model = robust_model,
    diagnostic_model = diagnostic_model,
    formula = model_formula,
    model_data = prepared_data$data,
    se_type = se_type,
    removed_observations = prepared_data$removed_observations,
    original_observations = prepared_data$original_observations,
    complete_observations = prepared_data$complete_observations
  )
  
  return(result)
}
