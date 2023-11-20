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
  if (!"ggplot2" %in% installed.packages()) {
    install.packages("ggplot2")
  }
  library(ggplot2)
  
  if (nrow(comparison_results) == 0) {
    warning("Empty dataset provided.")
    return(ggplot())  # Return an empty ggplot object
  }
  
  # Check if necessary columns are present
  required_columns <- c("Variable", "Discrepancies")
  if (!all(required_columns %in% names(comparison_results))) {
    warning("Data does not contain the required columns: Variable, Discrepancies")
    return(NULL)
  }
  
  ggplot(data = comparison_results, aes(x = Variable, y = Discrepancies)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Discrepancies per Variable", x = "Variable", y = "Count of Discrepancies")
}
