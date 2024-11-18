#' Shiny App for Viewing CDR Job Estimation Results
#'
#' Launches a Shiny app to interactively view CDR job estimation results.
#'
#' @param results A list of datasets to visualize.
#' @examples
#' run_shiny_app(results)
#' @export
run_shiny_app <- function(results) {
  library(shiny)
  library(ggplot2)
  library(dplyr)

  ui <- fluidPage(
    titlePanel("CDR Job Estimation Results Viewer"),
    sidebarLayout(
      sidebarPanel(
        selectInput("dataset", "Select Dataset:", choices = names(results)),
        selectInput("type", "Select Visualization Type:", choices = c("total_year", "cum_tech", "cum_total", "tech_year")),
        selectInput("job_metric", "Select Job Metric:", choices = c("mean_Jobs", "min_Jobs", "max_Jobs")),
        numericInput("ncol", "Number of Columns for Facets:", value = 2, min = 1),
        numericInput("nrow", "Number of Rows for Facets:", value = 1, min = 1)
      ),
      mainPanel(
        plotOutput("jobPlot")
      )
    )
  )

  server <- function(input, output) {
    output$jobPlot <- renderPlot({
      req(input$dataset)
      data <- results[[input$dataset]]
      visualize_results(
        data,
        type = input$type,
        job_metric = input$job_metric,
        ncol = input$ncol,
        nrow = input$nrow
      )
    })
  }

  shinyApp(ui = ui, server = server)
}
