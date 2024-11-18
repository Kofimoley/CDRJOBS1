#' Visualize Results
#'
#' Creates faceted plots for CDR job estimation results.
#'
#' @param data The dataset to visualize.
#' @param type The type of visualization. One of "total_year" or "cum_tech" or "tech_year" or "cum_total".
#' @param job_metric The metric to visualize. One of "mean_Jobs", "min_Jobs", "max_Jobs".
#' @param selected_scenarios A vector of scenario names to include, or NULL for all.
#' @param selected_regions A vector of region names to include, or NULL for all.
#' @param selected_years A vector of years to include (for "total_year"), or NULL for all.
#' @param ncol Number of columns for the facets.
#' @param nrow Number of rows for the facets. If NULL, rows are determined dynamically by ggplot2.
#' @param output_path The directory where plots will be saved as PNG files.
#' @examples
#' visualize_results(data = results$Job_total_year, type = "total_year", job_metric = "mean_Jobs")
visualize_results <- function(data, type = c("total_year", "cum_tech", "tech_year", "cum_total"), job_metric = c("mean_Jobs", "min_Jobs", "max_Jobs"), ncol = 2, nrow = NULL, output_path = getwd()) {
  type <- match.arg(type)
  job_metric <- match.arg(job_metric)

  if (!dir.exists(output_path)) stop("The specified output_path does not exist.")

  if (type == "total_year") {
    p <- ggplot(data, aes(x = year, y = .data[[job_metric]], fill = scenario)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~region, ncol = ncol, nrow = nrow) +
      theme_minimal() +
      labs(title = "Total Jobs by Year", x = "Year", y = paste(job_metric, "(Jobs)"))
  } else if (type == "cum_tech") {
    p <- ggplot(data, aes(x = technology, y = .data[[job_metric]], fill = scenario)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~region, ncol = ncol, nrow = nrow) +
      theme_minimal() +
      labs(title = "Cumulative Jobs by Technology", x = "Technology", y = paste(job_metric, "(Jobs)"))
  } else if (type == "tech_year") {
    p <- ggplot(data, aes(x = year, y = .data[[job_metric]], fill = technology)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~region, ncol = ncol, nrow = nrow) +
      theme_minimal() +
      labs(title = "Jobs by Technology by Year", x = "Year", y = paste(job_metric, "(Jobs)"))
  } else if (type == "cum_total") {
    p <- ggplot(data, aes(x = year, y = .data[[job_metric]], fill = scenario)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~region, ncol = ncol, nrow = nrow) +
      theme_minimal() +
      labs(title = "Cumulative Total Jobs by Year", x = "Year", y = paste(job_metric, "(Jobs)"))
  }

  ggsave(filename = file.path(output_path, paste0(type, "_plot.png")), plot = p, width = 10, height = 8)
}
