library(shiny)

source("R/data_functions.R")
source("R/model_functions.R")
source("R/table_functions.R")

economic_example <- readr::read_csv(
  "data/economic_example.csv",
  show_col_types = FALSE
)

ui <- fluidPage(
  titlePanel(
    "Interactive Diagnostics for Linear Models"
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "dataset",
        label = "Dataset",
        choices = c(
          "Economic example (wage1)" = "economic_example"
        ),
        selected = "economic_example"
      ),
      selectInput(
        inputId = "dependent_var",
        label = "Dependent variable",
        choices = names(economic_example),
        selected = "lwage"
      ),
      selectInput(
        inputId = "independent_vars",
        label = "Independent variables",
        choices = names(economic_example),
        selected = c(
          "educ",
          "exper",
          "tenure"
        ),
        multiple = TRUE
      ),
      selectInput(
        inputId = "se_type",
        label = "Standard error type",
        choices = c(
          "Classical" = "classical",
          "HC1" = "HC1",
          "HC2" = "HC2",
          "HC3" = "HC3"
        ),
        selected = "HC3"
      ),
      actionButton(
        inputId = "estimate_model",
        label = "Estimate Model"
      )
    ),
    mainPanel(
      gt::gt_output(
        "coefficient_table"
      )
    )
  )
)

server <- function(input, output) {
  model_result <- eventReactive(
    input$estimate_model,
    {
      run_regression(
        data = economic_example,
        dependent_var = input$dependent_var,
        independent_vars = input$independent_vars,
        se_type = input$se_type
      )
    }
  )
  output$coefficient_table <- gt::render_gt({
    result <- model_result()
    create_coefficient_table(
      result$robust_model
    )
  })
}

shinyApp(ui, server)
