% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_cdr_jobs.R
\name{calculate_cdr_jobs}
\alias{calculate_cdr_jobs}
\title{Function to calculate CDR Jobs}
\usage{
calculate_cdr_jobs(
  db_path,
  db_name,
  dat_file,
  scenario_list,
  region_list = NULL,
  output_path,
  output_type = c("csv", "list"),
  create_plots = TRUE,
  job_metric = "mean_Jobs",
  ncol = 2,
  nrow = NULL,
  selected_years = NULL
)
}
\arguments{
\item{db_path}{Path to the GCAM database.}

\item{db_name}{Name of the GCAM database.}

\item{dat_file}{Name of the .dat file to load.}

\item{scenario_list}{List of scenarios to include.}

\item{region_list}{List of regions to filter, default is all regions.}

\item{output_path}{Path to save output files.}

\item{output_type}{Output format, either "csv" or "list".}

\item{create_plots}{Logical, if TRUE, generates plots for the results.}

\item{job_metric}{The metric to visualize in plots. One of "mean_Jobs", "min_Jobs", "max_Jobs".}

\item{ncol}{Number of columns for the facets in the plots.}

\item{nrow}{Number of rows for the facets in the plots.}

\item{selected_years}{For "total_year", a vector of years to include, or NULL for all.}
}
\value{
A list containing the estimated jobs.
}
\description{
This function estimates the number of jobs created by CDR technologies based on a given GCAM database.
}
