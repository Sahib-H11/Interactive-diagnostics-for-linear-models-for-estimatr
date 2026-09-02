# Advanced R Final Project
# Week 3: Table functions
#
# Purpose:
# Create formatted regression result tables
# from the main lm_robust model.


# Create coefficient table
create_coefficient_table <- function(robust_model) {
  
  if (!inherits(robust_model, "lm_robust")) {
    stop(
      "The robust model must be an lm_robust object.",
      call. = FALSE
    )
  }
  
  coefficient_data <- broom::tidy(
    robust_model,
    conf.int = TRUE
  )
  
  table_data <- coefficient_data[
    ,
    c(
      "term",
      "estimate",
      "std.error",
      "conf.low",
      "conf.high",
      "p.value"
    )
  ]
  
  coefficient_table <- gt::gt(table_data)
  
  coefficient_table <- gt::cols_label(
    coefficient_table,
    term = "Term",
    estimate = "Estimate",
    std.error = "Std. Error",
    conf.low = "CI Lower",
    conf.high = "CI Upper",
    p.value = "p-value"
  )
  
  coefficient_table <- gt::fmt_number(
    coefficient_table,
    columns = c(
      estimate,
      std.error,
      conf.low,
      conf.high
    ),
    decimals = 3
  )
  
  coefficient_table <- gt::fmt(
    coefficient_table,
    columns = p.value,
    fns = function(x) {
      ifelse(
        x < 0.001,
        "<0.001",
        formatC(
          x,
          format = "f",
          digits = 3
        )
      )
    }
  )
  coefficient_table <- gt::tab_style(
    coefficient_table,
    style = list(
      gt::cell_fill(
        color = "#EEF3F7"
      ),
      gt::cell_text(
        weight = "bold",
        color = "#1a2b47"
      ),
      gt::cell_borders(
        sides = "bottom",
        color = "#E67E22",
        weight = gt::px(2)
      )
    ),
    locations = gt::cells_column_labels(
      columns = c(
        term,
        estimate,
        std.error,
        conf.low,
        conf.high,
        p.value
      )
    )
  )
  
  coefficient_table <- gt::tab_style(
    coefficient_table,
    style = list(
      gt::cell_fill(
        color = "#EEF3F7"
      ),
      gt::cell_text(
        weight = "bold",
        color = "#1a2b47"
      )
    ),
    locations = gt::cells_body(
      columns = term
    )
  )
  return(coefficient_table)
}


