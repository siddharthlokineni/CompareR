#' Generate a Summary Report of Dataset Comparison
#'
#' Provides a summary of the comparison results, highlighting key points such as the number of differing observations and variables.
#'
#' @param comparison_results A list containing the results of dataset comparisons.
#' @param detail_level The level of detail ('high', 'medium', 'low') for the summary.
#' @param output_format Format of the output ('text', 'html', or 'pdf').
#' @param file_name Name of the file to save the report to (applicable for 'html' and 'pdf' formats).
#' @return The summary report, output format depends on 'output_format' parameter.
#' @export
#' @examples
#' generate_summary_report(comparison_results, detail_level = "high", output_format = "text")


generate_summary_report <- function(comparison_results, detail_level = "high", output_format = "text", file_name = "summary_report") {
  summary_report <- paste("Summary Comparison Report\n", "======================\n\n", sep = "")
  
  # Generate summary based on the detail level
  if (!is.null(comparison_results$VariableDifferences)) {
    num_var_diffs <- length(comparison_results$VariableDifferences)
    summary_report <- paste0(summary_report, "Number of Variable Differences: ", num_var_diffs, "\n")
  }
  
  if (!is.null(comparison_results$ObservationDifferences)) {
    num_obs_diffs <- sum(sapply(comparison_results$ObservationDifferences, nrow))
    summary_report <- paste0(summary_report, "Total Number of Observation Differences: ", num_obs_diffs, "\n")
  }  
  # Output the summary based on the specified format
  if (output_format == "text") {
    cat(summary_report)
  } else if (output_format == "html") {
    rmarkdown::render(input = summary_report, output_format = "html_document", output_file = paste0(file_name, ".html"))
  } else if (output_format == "pdf") {
    rmarkdown::render(input = summary_report, output_format = "pdf_document", output_file = paste0(file_name, ".pdf"))
  } else {
    stop("Unsupported output format")
  }
}