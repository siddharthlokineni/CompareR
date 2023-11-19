#' Generate a Detailed Report of Dataset Comparison
#'
#' Creates a detailed report outlining all the differences found in the comparison,
#' including variable differences, observation differences, and group-based discrepancies.
#'
#' @param comparison_results A list containing the results of dataset comparisons.
#' @param output_format Format of the output ('text', 'html', or 'pdf').
#' @param file_name Name of the file to save the report to (applicable for 'html' and 'pdf' formats).
#' @return The detailed report, output format depends on 'output_format' parameter.
#' @export
#' @examples
#' generate_detailed_report(comparison_results, output_format = "text")

generate_detailed_report <- function(comparison_results, output_format = "text", file_name = "detailed_report") {
  detailed_report <- paste("Detailed Comparison Report\n", "======================\n\n", sep = "")
  
  # Generate report content
  if (!is.null(comparison_results$VariableDifferences)) {
    detailed_report <- paste0(detailed_report, "Variable Differences:\n")
    detailed_report <- paste0(detailed_report, format(comparison_results$VariableDifferences), "\n\n")
  }
  
  if (!is.null(comparison_results$ObservationDifferences)) {
    detailed_report <- paste0(detailed_report, "Observation Differences:\n")
    for (col in names(comparison_results$ObservationDifferences)) {
      detailed_report <- paste0(detailed_report, "Column: ", col, "\n")
      detailed_report <- paste0(detailed_report, format(comparison_results$ObservationDifferences[[col]]), "\n\n")
    }
  }
  
  # Output the report based on the specified format
  if (output_format == "text") {
    cat(detailed_report)
  } else if (output_format == "html") {
    rmarkdown::render(input = detailed_report, output_format = "html_document", output_file = paste0(file_name, ".html"))
  } else if (output_format == "pdf") {
    rmarkdown::render(input = detailed_report, output_format = "pdf_document", output_file = paste0(file_name, ".pdf"))
  } else {
    stop("Unsupported output format")
  }
}