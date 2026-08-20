# Advanced R Final Project
# Week 3: Backend test cases
#
# Purpose:
# Check that the regression backend works correctly
# under several important model and data scenarios.


# Load project functions
source("R/data_functions.R")
source("R/model_functions.R")
source("R/diagnostic_functions.R")
source("R/plotting_functions.R")
source("R/table_functions.R")


# Test Case 1: Valid model with multiple predictors
test_model <- run_regression(
  data = mtcars,
  dependent_var = "mpg",
  independent_vars = c("wt", "hp"),
  se_type = "HC3"
)

print(test_model$formula)
print(test_model$complete_observations)


# Test Case 2: Missing values
missing_data <- mtcars

missing_data$mpg[1] <- NA
missing_data$wt[2] <- NA

missing_model <- run_regression(
  data = missing_data,
  dependent_var = "mpg",
  independent_vars = c("wt", "hp"),
  se_type = "HC3"
)

print(missing_model$removed_observations)
print(missing_model$complete_observations)


# Test Case 3: Valid model with one predictor
single_predictor_model <- run_regression(
  data = mtcars,
  dependent_var = "mpg",
  independent_vars = "wt",
  se_type = "HC3"
)

single_predictor_vif <- calculate_vif(
  single_predictor_model$diagnostic_model
)

print(single_predictor_model$complete_observations)
print(single_predictor_vif$message)


# Test Case 4: Invalid variable selection
invalid_selection_message <- tryCatch(
  {
    run_regression(
      data = mtcars,
      dependent_var = "mpg",
      independent_vars = c("mpg", "wt"),
      se_type = "HC3"
    )
    "No error was produced."
  },
  error = function(e) {
    e$message
  }
)

print(invalid_selection_message)


# Test Case 5: Insufficient observations
insufficient_observations_message <- tryCatch(
  {
    small_data <- mtcars[1:3, ]
    
    run_regression(
      data = small_data,
      dependent_var = "mpg",
      independent_vars = c("wt", "hp"),
      se_type = "HC3"
    )
    
    "No error was produced."
  },
  error = function(e) {
    e$message
  }
)

print(insufficient_observations_message)


# Test Case 6: Non-numeric dependent variable
non_numeric_data <- mtcars
non_numeric_data$mpg <- as.character(non_numeric_data$mpg)

non_numeric_message <- tryCatch(
  {
    run_regression(
      data = non_numeric_data,
      dependent_var = "mpg",
      independent_vars = c("wt", "hp"),
      se_type = "HC3"
    )
    
    "No error was produced."
  },
  error = function(e) {
    e$message
  }
)

print(non_numeric_message)


# Test Case 7: Variable not found in dataset
missing_variable_message <- tryCatch(
  {
    run_regression(
      data = mtcars,
      dependent_var = "mpg",
      independent_vars = c("wt", "not_a_variable"),
      se_type = "HC3"
    )
    
    "No error was produced."
  },
  error = function(e) {
    e$message
  }
)

print(missing_variable_message)


# Test Case 8: Diagnostic functions on a valid model
bp_result <- calculate_bp_test(
  test_model$diagnostic_model
)

vif_result <- calculate_vif(
  test_model$diagnostic_model
)

residual_plot <- plot_residuals_vs_fitted(
  test_model$diagnostic_model
)

qq_plot <- plot_qq(
  test_model$diagnostic_model
)

coefficient_table <- create_coefficient_table(
  test_model$robust_model
)

print(bp_result$method)
print(vif_result$values)
print(inherits(residual_plot, "ggplot"))
print(inherits(qq_plot, "ggplot"))
print(inherits(coefficient_table, "gt_tbl"))


# Test Case 9: Unsupported standard error type
unsupported_se_message <- tryCatch(
  {
    run_regression(
      data = mtcars,
      dependent_var = "mpg",
      independent_vars = c("wt", "hp"),
      se_type = "not_valid"
    )
    
    "No error was produced."
  },
  error = function(e) {
    e$message
  }
)

print(unsupported_se_message)


# Test Case 10: Non-numeric independent variable
non_numeric_predictor_data <- mtcars
non_numeric_predictor_data$hp <- as.character(
  non_numeric_predictor_data$hp
)

non_numeric_predictor_message <- tryCatch(
  {
    run_regression(
      data = non_numeric_predictor_data,
      dependent_var = "mpg",
      independent_vars = c("wt", "hp"),
      se_type = "HC3"
    )
    
    "No error was produced."
  },
  error = function(e) {
    e$message
  }
)

print(non_numeric_predictor_message)


# Test Case 11: No dependent variable selected
missing_dependent_message <- tryCatch(
  {
    run_regression(
      data = mtcars,
      dependent_var = "",
      independent_vars = c("wt", "hp"),
      se_type = "HC3"
    )
    
    "No error was produced."
  },
  error = function(e) {
    e$message
  }
)

print(missing_dependent_message)


# Test Case 12: No independent variable selected
missing_independent_message <- tryCatch(
  {
    run_regression(
      data = mtcars,
      dependent_var = "mpg",
      independent_vars = character(0),
      se_type = "HC3"
    )
    
    "No error was produced."
  },
  error = function(e) {
    e$message
  }
)

print(missing_independent_message)


# TEST 13: Controlled model estimation failure
cat("\n=== TEST 13: Controlled model estimation failure ===\n")

invalid_model_data <- mtcars
invalid_model_data$hp[1] <- Inf

model_error_message <- tryCatch(
  run_regression(
    data = invalid_model_data,
    dependent_var = "mpg",
    independent_vars = c("wt", "hp"),
    se_type = "HC3"
  ),
  error = function(e) {
    conditionMessage(e)
  }
)

print(model_error_message)

stopifnot(
  startsWith(
    model_error_message,
    "Diagnostic model estimation failed:"
  )
)

cat("\nTEST 13 PASSED\n")

