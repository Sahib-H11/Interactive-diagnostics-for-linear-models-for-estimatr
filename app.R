library(shiny)

source("R/data_functions.R")
source("R/model_functions.R")
source("R/diagnostic_functions.R")
source("R/plotting_functions.R")
source("R/table_functions.R")
source("R/s3_methods.R")

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
      h3("Data"),
      selectInput(
        inputId = "dataset",
        label = "Dataset",
        choices = c(
          "Economic example (wage1)" = "economic_example",
          "Upload CSV file" = "upload_csv"
        ),
        selected = "economic_example"
      ),
      
      conditionalPanel(
        condition = "input.dataset == 'upload_csv'",
        fileInput(
          inputId = "csv_file",
          label = "Choose CSV file",
          accept = ".csv"
        )
      ),
      h3("Model"),
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
      h2("Results"),
      h3("Model Information"),
      verbatimTextOutput("model_info"),
      h3("Coefficient Results"),
      gt::gt_output("coefficient_table"),
      
      h3("Diagnostics"),
      h4("Studentized Breusch-Pagan Test"),
      verbatimTextOutput("bp_test"),
      h4("Variance Inflation Factors"),
      verbatimTextOutput("vif_result"),
      h4("Residuals vs Fitted"),
      plotOutput("residual_plot"),
      h4("Normal QQ Plot"),
      plotOutput("qq_plot")
    )
  )
)

server <- function(input, output, session) {
  
  selected_data <- reactive({
    
    if (input$dataset == "economic_example") {
      return(economic_example)
    }
    
    req(input$csv_file)
    
    uploaded_data <- tryCatch(
      readr::read_csv(
        input$csv_file$datapath,
        show_col_types = FALSE
      ),
      error = function(e) {
        stop(
          "The uploaded file could not be analyzed.",
          call. = FALSE
        )
      }
    )
    
    return(uploaded_data)
  })
  
  
  observeEvent(
    selected_data(),
    {
      data <- selected_data()
      
      numeric_vars <- names(data)[
        vapply(
          data,
          is.numeric,
          logical(1)
        )
      ]
      
      if (length(numeric_vars) == 0L) {
        
        updateSelectInput(
          session,
          "dependent_var",
          choices = character(0)
        )
        
        updateSelectInput(
          session,
          "independent_vars",
          choices = character(0)
        )
        
        return()
      }
      
      if (input$dataset == "economic_example") {
        
        dependent_selected <- "lwage"
        
        independent_selected <- c(
          "educ",
          "exper",
          "tenure"
        )
        
      } else {
        
        dependent_selected <- numeric_vars[1]
        
        independent_selected <- head(
          setdiff(
            numeric_vars,
            dependent_selected
          ),
          3
        )
      }
      
      updateSelectInput(
        session,
        "dependent_var",
        choices = numeric_vars,
        selected = dependent_selected
      )
      
      updateSelectInput(
        session,
        "independent_vars",
        choices = numeric_vars,
        selected = independent_selected
      )
    }
  )
  
  
  model_result <- eventReactive(
    input$estimate_model,
    {
      regression_model <- run_regression(
        data = selected_data(),
        dependent_var = input$dependent_var,
        independent_vars = input$independent_vars,
        se_type = input$se_type
      )
      create_regression_result(
        regression_model
      )
    }
  )
  
  output$model_info <- renderText({
    
    result <- model_result()
    
    model_stats <- broom::glance(
      result$robust_model
    )
    
    paste(
      "Formula:",
      paste(
        deparse(result$settings$formula),
        collapse = " "
      ),
      "\nStandard error type:",
      result$settings$se_type,
      "\nObservations used:",
      result$data_info$complete_observations,
      "\nRemoved observations:",
      result$data_info$removed_observations,
      "\nR-squared:",
      round(model_stats$r.squared, 3),
      "\nAdjusted R-squared:",
      round(model_stats$adj.r.squared, 3)
    )
  })
  
  output$coefficient_table <- gt::render_gt({
    result <- model_result()
    result$coefficients
  })
  output$bp_test <- renderText({
    result <- model_result()
    bp <- result$diagnostics$bp_test
    paste(
      "Statistic:",
      round(bp$statistic, 3),
      "\nDegrees of freedom:",
      bp$degrees_of_freedom,
      "\np-value:",
      round(bp$p_value, 4)
    )
  })
  output$vif_result <- renderPrint({
    result <- model_result()
    vif <- result$diagnostics$vif
    if (is.null(vif$values)) {
      cat(vif$message)
    } else {
      print(
        vif$values,
        row.names = FALSE
      )
    }
  })
  output$residual_plot <- renderPlot({
    result <- model_result()
    result$plots$residuals_vs_fitted
  })
  output$qq_plot <- renderPlot({
    result <- model_result()
    result$plots$qq
  })
  }

shinyApp(ui, server)
