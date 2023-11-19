#' Reset Comparison Settings to Defaults
#'
#' Resets the comparison settings to their default values. 
#' This includes resetting the tolerance level and the method for handling missing values.
#'
#' @return None; this function resets global options and does not return a value.
#' @export
#' @examples
#' reset_comparison_settings()

reset_comparison_settings <- function() {
  options(comparison_tolerance = 0)
  options(missing_value_handling = "ignore")
  message("Comparison settings have been reset to default values.")
}