install.packages("shiny")

library(shiny)

ui <- fluidPage(
  titlePanel("Interactive Diagnostics for Linear Models")
)

server <- function(input, output){
  
}

shinyApp(ui, server)
