library(shiny)

source("R/data_functions.R")
source("R/model_functions.R")
source("R/diagnostic_functions.R")
source("R/plotting_functions.R")
source("R/s3_methods.R")
source("R/table_functions.R")

economic_example <- readr::read_csv(
  "data/economic_example.csv",
  show_col_types = FALSE
)

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: #f4f5f7;
          color: #333333;
        }
        .app-header {
  text-align: center;
  margin-bottom: 22px;
}

.app-header h1 {
  margin-bottom: 5px;
  font-weight: 600;
  color: #1a2b47;
}

.app-header p {
  margin-top: 0;
  color: #C9792B;
  font-size: 16px;
  font-weight: 500;
}

        #estimate_model {
  width: 100%;
  max-width: 310px;
  background-color: #E67E22;
  color: #ffffff;
  border: none;
  border-radius: 5px;
  font-weight: 600;
  padding: 10px 12px;
  margin-top: 8px;
}

#estimate_model:hover {
  background-color: #C96A17;
  color: #ffffff;
}
.result-card {
  background-color: #EEF3F7;
  border: 1px solid #e0e3e7;
  border-radius: 7px;
  padding: 18px 20px;
  margin-bottom: 22px;
}
.model-info-card {
  background-color: #EEF3F7;
  border-left: 4px solid #E67E22;
}

.result-card h3 {
  margin-top: 0;
  margin-bottom: 14px;
  color: #1a2b47;
}

.result-card pre {
  background-color: transparent;
  border: none;
  padding: 0;
  margin: 0;
  font-family: inherit;
  font-size: 15px;
  line-height: 1.6;
  color: #333333;
}
.model-info-grid {
  display: grid;
  grid-template-columns: 190px 1fr;
  column-gap: 24px;
  row-gap: 10px;
  align-items: center;
}

.info-label {
  font-weight: 700;
  color: #003049;
}

.info-value {
  color: #333333;
}

.info-accent {
  color: #333333;
  font-weight: 400;
}
.setup-panel {
  background-color: #1a2b47;
  border-radius: 8px;
  padding: 22px;
  margin-bottom: 24px;
  color: #ffffff;
}

.setup-panel h3,
.setup-panel label {
  color: #ffffff;
}

.setup-panel h3 {
  margin-top: 0;
}

.section-title {
  color: #1a2b47;
  font-weight: 700;
  margin-bottom: 18px;
  padding-bottom: 8px;
  border-bottom: 3px solid #E67E22;
}

.coefficient-card {
  background-color: #ffffff;
  border-left: 4px solid #E67E22;
}

.coefficient-table-wrap {
  max-width: 720px;
  margin: 0 auto;
}

.nav-tabs > li > a {
  color: #486581;
  font-weight: 500;
}

.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus {
  color: #1a2b47;
  font-weight: 700;
  background-color: #ffffff;
  border-bottom: 3px solid #E67E22;
}

.nav-tabs > li > a:hover {
  color: #1a2b47;
  background-color: #EEF3F7;
}

.diagnostic-card {
  border-left: 4px solid #E67E22;
  min-height: 190px;
}

.bp-card {
  background-color: #EEF3F7;
}
 
.vif-card {
  background-color: #EEF3F7;
}

.diagnostic-info-grid {
  display: grid;
  grid-template-columns: 160px 1fr;
  column-gap: 20px;
  row-gap: 10px;
  align-items: center;
}

.vif-table {
  width: 100%;
  max-width: 360px;
  border-collapse: collapse;
}

.vif-table th {
  color: #1a2b47;
  font-weight: 700;
  text-align: left;
  padding: 6px 10px;
  border-bottom: 2px solid #E67E22;
}

.vif-table td {
  padding: 7px 10px;
  border-bottom: 1px solid #e0e3e7;
  color: #333333;
}

.vif-table td:first-child {
  font-weight: 600;
  color: #1a2b47;
}

.diagnostic-plot-card {
  background-color: #ffffff;
  border-left: 4px solid #E67E22;
  padding: 12px 14px;
}
      ")
    )
  ),
  div(
    class = "app-header",
    h1("Interactive Diagnostics for Linear Models"),
    tags$p(
      "Regression estimation and diagnostics using estimatr"
    )
  ),
  tabsetPanel(
    id = "main_tabs",
    type = "tabs",
    
    tabPanel(
      title = "Setup",
      
      h2(
        class = "section-title",
        "Analysis Setup"
      ),
      div(
        class = "setup-panel",
      fluidRow(
        
        column(
          width = 4,
          
          h3("Data Source"),
          
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
          )
        ),
        
        column(
          width = 8,
          
          h3("Model Specification"),
          
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
        )
      )
      )
    ),
    
    tabPanel(
      title = "Results",
      
      h2(
        class = "section-title",
        "Results"
      ),
      
      div(
        class = "result-card model-info-card",
        h3("Model Information"),
        uiOutput("model_info")
      ),
      
      div(
        class = "result-card coefficient-card",
        h3("Coefficient Results"),
        div(
          class = "coefficient-table-wrap",
          gt::gt_output("coefficient_table")
        )
      )
    ),
    
    tabPanel(
      title = "Diagnostics",
      
      h2(
        class = "section-title",
        "Diagnostics"
      ),
      
      fluidRow(
        column(
          width = 6,
          div(
            class = "result-card diagnostic-card bp-card",
            h3("Studentized Breusch-Pagan Test"),
            uiOutput("bp_test")
          )
        ),
        
        column(
          width = 6,
          div(
            class = "result-card diagnostic-card vif-card",
            h3("Variance Inflation Factors"),
            uiOutput("vif_result")
          )
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          div(
            class = "result-card diagnostic-plot-card",
            plotOutput(
              "residual_plot",
              height = "360px"
            )
          )
        ),
        
        column(
          width = 6,
          div(
            class = "result-card diagnostic-plot-card",
            plotOutput(
              "qq_plot",
              height = "360px"
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(
    input$dataset,
    {
      if (input$dataset == "upload_csv") {
        
        updateSelectInput(
          session,
          "dependent_var",
          choices = character(0),
          selected = character(0)
        )
        
        updateSelectInput(
          session,
          "independent_vars",
          choices = character(0),
          selected = character(0)
        )
      }
    }
  )
  
  selected_data <- reactive({
    
    if (input$dataset == "economic_example") {
      return(economic_example)
    }
    
    req(input$csv_file)
    
    uploaded_data <- tryCatch(
      {
        data <- readr::read_csv(
          input$csv_file$datapath,
          show_col_types = FALSE
        )
        
        if (
          nrow(data) == 0L ||
          ncol(data) == 0L
        ) {
          stop("Invalid or empty CSV file.")
        }
        
        data
      },
      error = function(e) {
        
        showNotification(
          "The uploaded file could not be analyzed.",
          type = "error",
          duration = 5
        )
        
        return(NULL)
      }
    )
    
    req(uploaded_data)
    
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
      
      if (
        input$dataset == "upload_csv" &&
        is.null(input$csv_file)
      ) {
        
        showNotification(
          "Please upload a CSV file before estimating the model.",
          type = "error",
          duration = 5
        )
        
        req(input$csv_file)
      }
      
      tryCatch(
        {
          current_data <- selected_data()
          
          numeric_vars <- names(current_data)[
            vapply(
              current_data,
              is.numeric,
              logical(1)
            )
          ]
          
          if (length(numeric_vars) == 0L) {
            stop(
              "The uploaded dataset does not contain any numeric variables.",
              call. = FALSE
            )
          }
          
          regression_model <- run_regression(
            data = current_data,
            dependent_var = input$dependent_var,
            independent_vars = input$independent_vars,
            se_type = input$se_type
          )
          
          create_regression_result(
            regression_model
          )
        },
        error = function(e) {
          
          showNotification(
            conditionMessage(e),
            type = "error",
            duration = 5
          )
          
          req(FALSE)
        }
      )
    }
  )
  observeEvent(
    model_result(),
    {
      updateTabsetPanel(
        session = session,
        inputId = "main_tabs",
        selected = "Results"
      )
      
      showNotification(
        paste(
          "Model estimated successfully.",
          "Review the results below or open the",
          "Diagnostics tab for diagnostic checks."
        ),
        type = "message",
        duration = 7
      )
    }
  )
  
  output$model_info <- renderUI({
    
    result <- model_result()
    
    model_stats <- broom::glance(
      result$robust_model
    )
    
    div(
      class = "model-info-grid",
      
      div(class = "info-label", "Formula"),
      div(
        class = "info-value",
        paste(
          deparse(result$settings$formula),
          collapse = " "
        )
      ),
      
      div(class = "info-label", "Standard error type"),
      div(
        class = "info-value info-accent",
        result$settings$se_type
      ),
      
      div(class = "info-label", "Observations used"),
      div(
        class = "info-value",
        result$data_info$complete_observations
      ),
      
      div(class = "info-label", "Removed observations"),
      div(
        class = "info-value",
        result$data_info$removed_observations
      ),
      
      div(class = "info-label", "R-squared"),
      div(
        class = "info-value info-accent",
        round(model_stats$r.squared, 3)
      ),
      
      div(class = "info-label", "Adjusted R-squared"),
      div(
        class = "info-value info-accent",
        round(model_stats$adj.r.squared, 3)
      )
    )
  })
  
  output$coefficient_table <- gt::render_gt({
    result <- model_result()
    result$coefficients
  })
  output$bp_test <- renderUI({
    
    result <- model_result()
    bp <- result$diagnostics$bp_test
    
    div(
      class = "diagnostic-info-grid",
      
      div(class = "info-label", "Statistic"),
      div(
        class = "info-value",
        round(bp$statistic, 3)
      ),
      
      div(class = "info-label", "Degrees of freedom"),
      div(
        class = "info-value",
        bp$degrees_of_freedom
      ),
      
      div(class = "info-label", "p-value"),
      div(
        class = "info-value",
        round(bp$p_value, 4)
      )
    )
  })
  output$vif_result <- renderUI({
    
    result <- model_result()
    vif <- result$diagnostics$vif
    
    if (is.null(vif$values)) {
      
      div(
        class = "info-value",
        vif$message
      )
      
    } else {
      
      tags$table(
        class = "vif-table",
        
        tags$thead(
          tags$tr(
            tags$th("Variable"),
            tags$th("VIF")
          )
        ),
        
        tags$tbody(
          lapply(
            seq_len(nrow(vif$values)),
            function(i) {
              tags$tr(
                tags$td(vif$values$variable[i]),
                tags$td(
                  round(
                    vif$values$vif[i],
                    3
                  )
                )
              )
            }
          )
        )
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
