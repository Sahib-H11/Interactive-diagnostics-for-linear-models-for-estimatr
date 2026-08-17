# Advanced R Final Project
# Week 2: Data preparation functions
#
# Purpose:
# Check the input dataset, validate variable selections,
# and prepare complete observations for model estimation.


# Check whether the supplied dataset is usable
check_data <- function(data) {
  
  if (is.null(data)) {
    stop(
      "No dataset was provided.",
      call. = FALSE
    )
  }
  
  if (!is.data.frame(data)) {
    stop(
      "The provided object must be a data frame.",
      call. = FALSE
    )
  }
  
  if (nrow(data) == 0L) {
    stop(
      "The dataset does not contain any observations.",
      call. = FALSE
    )
  }
  
  if (ncol(data) == 0L) {
    stop(
      "The dataset does not contain any variables.",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


# Validate the selected dependent and independent variables
validate_variables <- function(
    data,
    dependent_var,
    independent_vars
) {
  
  check_data(data)
  
  if (
    is.null(dependent_var) ||
    length(dependent_var) != 1L ||
    is.na(dependent_var) ||
    dependent_var == ""
  ) {
    stop(
      "Please select a dependent variable.",
      call. = FALSE
    )
  }
  
  if (
    is.null(independent_vars) ||
    length(independent_vars) == 0L
  ) {
    stop(
      "Please select at least one independent variable.",
      call. = FALSE
    )
  }
  
  selected_variables <- c(
    dependent_var,
    independent_vars
  )
  
  missing_variables <- setdiff(
    selected_variables,
    names(data)
  )
  
  if (length(missing_variables) > 0L) {
    stop(
      paste(
        "Missing variables:",
        paste(
          missing_variables,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  if (dependent_var %in% independent_vars) {
    stop(
      paste(
        "The dependent variable cannot also be used",
        "as an independent variable."
      ),
      call. = FALSE
    )
  }
  
  if (!is.numeric(data[[dependent_var]])) {
    stop(
      "The selected dependent variable must be numeric.",
      call. = FALSE
    )
  }
  non_numeric_independent_vars <- independent_vars[
    !vapply(
      data[independent_vars],
      is.numeric,
      logical(1)
    )
  ]
  
  if (length(non_numeric_independent_vars) > 0L) {
    stop(
      paste(
        "The selected independent variables must be numeric:",
        paste(
          non_numeric_independent_vars,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


# Prepare data using only the variables selected for the model
prepare_model_data <- function(
    data,
    dependent_var,
    independent_vars
) {
  
  check_data(data)
  
  validate_variables(
    data = data,
    dependent_var = dependent_var,
    independent_vars = independent_vars
  )
  
  selected_variables <- c(
    dependent_var,
    independent_vars
  )
  
  model_data <- data[
    ,
    selected_variables,
    drop = FALSE
  ]
  
  complete_rows <- stats::complete.cases(model_data)
  
  cleaned_data <- model_data[
    complete_rows,
    ,
    drop = FALSE
  ]
  
  minimum_observations <- length(independent_vars) + 2L
  
  if (nrow(cleaned_data) < minimum_observations) {
    stop(
      paste(
        "Not enough complete observations remain",
        "for model estimation."
      ),
      call. = FALSE
    )
  }
  
  result <- list(
    data = cleaned_data,
    removed_observations = sum(!complete_rows),
    original_observations = nrow(data),
    complete_observations = nrow(cleaned_data),
    variables = selected_variables
  )
  
  return(result)
}

