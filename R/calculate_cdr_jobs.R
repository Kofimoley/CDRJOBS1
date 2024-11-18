#' Function to calculate CDR Jobs
#'
#' This function estimates the number of jobs created by CDR technologies based on a given GCAM database.
#'
#' @param db_path Path to the GCAM database.
#' @param db_name Name of the GCAM database.
#' @param dat_file Name of the .dat file to load.
#' @param scenario_list List of scenarios to include.
#' @param region_list List of regions to filter, default is all regions.
#' @param output_path Path to save output files.
#' @param output_type Output format, either "csv" or "list".
#' @param create_plots Logical, if TRUE, generates plots for the results.
#' @param job_metric The metric to visualize in plots. One of "mean_Jobs", "min_Jobs", "max_Jobs".
#' @param ncol Number of columns for the facets in the plots.
#' @param nrow Number of rows for the facets in the plots.
#' @param selected_years For "total_year", a vector of years to include, or NULL for all.
#' @return A list containing the estimated jobs.
#' @import dplyr rgcam ggplot2
#' @export
calculate_cdr_jobs <- function(db_path, db_name, dat_file, scenario_list, region_list = NULL, output_path, output_type = c("csv", "list"), create_plots = TRUE, job_metric = "mean_Jobs", ncol = 2, nrow = NULL, selected_years = NULL) {
  output_type <- match.arg(output_type, several.ok = TRUE)
  if (!dir.exists(output_path)) stop("The specified output_path does not exist.")

  CDR_query <- '<?xml version="1.0"?>
  <queries>
  <aQuery>
  <all-regions/>
  <emissionsQueryBuilder title="CO2 sequestration by tech">
  <axis1 name="subsector">subsector</axis1>
  <axis2 name="Year">emissions-sequestered</axis2>
  <xPath buildList="true" dataName="emissions" group="false" sumAll="false">*[@type = "sector"]/*[@type="subsector"]/*[@type="technology"]//CO2/emissions-sequestered/node()</xPath></emissionsQueryBuilder></aQuery></queries>'
  query_file <- tempfile(fileext = ".xml")
  writeLines(CDR_query, query_file)

  CDR_tech <- addScenario(localDBConn(db_path, db_name), paste0(dat_file, ".dat"), scenario_list, query_file)
  CDR_Output <- getQuery(CDR_tech, "CO2 sequestration by tech")
  if (!is.null(region_list)) CDR_Output <- CDR_Output %>% filter(region %in% region_list)

  data("CDR_Job_Inten", package = "CDRJOBS1")

  joined_data <- dplyr::left_join(CDR_Output, CDR_Job_Inten, by = c("technology" = "CDR"))
  if (nrow(joined_data) == 0) stop("Join failed: technology column not found in the data.")

  calculate_jobs <- function(data) {
    data %>%
      group_by(across(-value)) %>%
      summarize(
        mean_Jobs = sum(mean_int * 3.667 * 10^6 * value, na.rm = TRUE),
        min_Jobs = sum(min_int * 3.667 * 10^6 * value, na.rm = TRUE),
        max_Jobs = sum(max_int * 3.667 * 10^6 * value, na.rm = TRUE),
        .groups = "drop"
      )
  }

  Job_total_year <- calculate_jobs(joined_data %>% group_by(scenario, region, year))
  if ("csv" %in% output_type) write.csv(Job_total_year, file.path(output_path, "Job_total_year.csv"), row.names = FALSE)
  results <- list(Job_total_year = Job_total_year)

  if (create_plots) {
    message("Generating and saving visualizations...")
    visualize_results(data = Job_total_year, type = "total_year", job_metric = job_metric, ncol = ncol, nrow = nrow, output_path = output_path)
  }

  if ("list" %in% output_type || length(output_type) == 0) return(results)
}
