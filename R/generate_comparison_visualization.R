#' Generate Visualization for Data Comparison
#'
#' Creates a visual representation of data comparison results. 
#' This can be particularly useful for showing discrepancies in variable distributions or counts.
#'
#' @param comparison_results A list containing the results of dataset comparisons.
#' @return A plot object visualizing the comparison results.
#' @export
#' @examples
#' generate_comparison_visualization(comparison_results)

generate_comparison_visualization <- function(comparison_results) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
    library(ggplot2)
  }
  
  # Validate input
  if (is.null(comparison_results) || !is.data.frame(comparison_results)) {
    stop("comparison_results must be a non-null data frame.")
  }
  if (nrow(comparison_results) == 0) {
    warning("Empty dataset provided.")
    return(ggplot())  # Return an empty ggplot object
  }
  
  # Check for required columns and suggest a default plot if missing
  required_columns <- c("Variable", "Discrepancies")
  if (!all(required_columns %in% names(comparison_results))) {
    warning("Data does not contain the required columns: Variable, Discrepancies. Attempting a default plot with available columns.")
    if (length(names(comparison_results)) >= 2) {
      comparison_results <- comparison_results[, 1:2]
      names(comparison_results) <- required_columns
    } else {
      stop("Not enough columns in comparison_results to create a default plot.")
    }
  }
  
  # Generate the plot
  ggplot(data = comparison_results, aes_string(x = required_columns[1], y = required_columns[2])) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Discrepancies per Variable", x = "Variable", y = "Count of Discrepancies")
}
