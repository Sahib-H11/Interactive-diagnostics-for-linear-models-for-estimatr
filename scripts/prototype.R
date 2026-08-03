# Advanced R Final Project
# Week 1: Regression prototype
#
# Purpose:
# Compare a standard linear model estimated with stats::lm()
# with models estimated using estimatr::lm_robust().
#
# Development dataset:
# mtcars

library(estimatr)
library(broom)

message("Week 1 packages loaded successfully.")

# Standard linear model using stats::lm()
model_lm <- stats::lm(
  mpg ~ wt + hp,
  data = mtcars
)

print(model_lm)

# Linear model using estimatr with classical standard errors
model_classical <- estimatr::lm_robust(
  mpg ~ wt + hp,
  data = mtcars,
  se_type = "classical"
)

print(model_classical)

# Linear model using estimatr with HC3 robust standard errors
model_hc3 <- estimatr::lm_robust(
  mpg ~ wt + hp,
  data = mtcars,
  se_type = "HC3"
)

print(model_hc3)

# Clean coefficient output for the standard lm model
tidy_lm <- broom::tidy(
  model_lm,
  conf.int = TRUE
)

print(tidy_lm, width = Inf)

# Clean coefficient output for the classical lm_robust model
tidy_classical <- broom::tidy(
  model_classical,
  conf.int = TRUE
)

print(tidy_classical)

# Clean coefficient output for the HC3 lm_robust model
tidy_hc3 <- broom::tidy(
  model_hc3,
  conf.int = TRUE
)

print(tidy_hc3)


# Model-level statistics for the standard lm model
glance_lm <- broom::glance(model_lm)

print(glance_lm, width = Inf)

# Model-level statistics for the classical lm_robust model
glance_classical <- broom::glance(model_classical)

print(glance_classical)

# Model-level statistics for the HC3 lm_robust model
glance_hc3 <- broom::glance(model_hc3)

print(glance_hc3)

# Fitted values and residuals for the standard lm model
fitted_lm <- stats::fitted(model_lm)
residuals_lm <- stats::residuals(model_lm)

print(head(fitted_lm))
print(head(residuals_lm))

print(length(fitted_lm))
print(length(residuals_lm))


# Compare coefficient results across the three models
comparison_table <- rbind(
  data.frame(
    model = "lm",
    tidy_lm[
      ,
      c(
        "term",
        "estimate",
        "std.error",
        "p.value",
        "conf.low",
        "conf.high"
      )
    ]
  ),
  data.frame(
    model = "lm_robust classical",
    tidy_classical[
      ,
      c(
        "term",
        "estimate",
        "std.error",
        "p.value",
        "conf.low",
        "conf.high"
      )
    ]
  ),
  data.frame(
    model = "lm_robust HC3",
    tidy_hc3[
      ,
      c(
        "term",
        "estimate",
        "std.error",
        "p.value",
        "conf.low",
        "conf.high"
      )
    ]
  )
)

print(comparison_table)

# Verify similarities and differences across models
comparison_checks <- data.frame(
  check = c(
    "lm and classical estimates are equal",
    "classical and HC3 estimates are equal",
    "lm and classical standard errors are equal",
    "classical and HC3 standard errors are equal"
  ),
  result = c(
    isTRUE(all.equal(
      tidy_lm$estimate,
      tidy_classical$estimate
    )),
    isTRUE(all.equal(
      tidy_classical$estimate,
      tidy_hc3$estimate
    )),
    isTRUE(all.equal(
      tidy_lm$std.error,
      tidy_classical$std.error
    )),
    isTRUE(all.equal(
      tidy_classical$std.error,
      tidy_hc3$std.error
    ))
  )
)

print(comparison_checks)

# Week 1 technical notes
technical_notes <- c(
  "estimatr::lm_robust() is the main estimation function in this project.",
  "stats::lm() is used as a standard comparison model and will later support diagnostics.",
  "Classical standard errors rely on the usual constant-variance assumption.",
  "HC3 standard errors are designed to be more robust to heteroskedasticity.",
  "The coefficient estimates remain unchanged because the data and model formula are unchanged.",
  "The standard errors, p-values, and confidence intervals can change because the uncertainty calculation changes."
)

print(technical_notes)

