#' Generate a Report of Differences Found in Dataset Comparison
#'
#' Summarizes the differences found between two datasets after comparison. 
#' This function creates a report detailing variable and observation differences.
#'
#' @param variable_diffs A data frame or list detailing the differences found in variables.
#' @param observation_diffs A data frame or list detailing the differences found in observations.
#' @return A structured report of the differences, typically a list or a data frame.
#' @export
#' @examples
#' # Assuming variable_diffs and observation_diffs are obtained from comparison functions
#' report_differences(variable_diffs, observation_diffs)

report_differences <- function(variable_diffs, observation_diffs) {
  report <- list()
  
  if (!is.null(variable_diffs)) {
    report$variable_differences <- variable_diffs
  }
  
  if (!is.null(observation_diffs)) {
    report$observation_differences <- observation_diffs
  }
  
  report
}