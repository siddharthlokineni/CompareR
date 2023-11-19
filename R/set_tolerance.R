#' Set Tolerance Level for Comparisons
#'
#' Defines the tolerance level for numeric comparisons. 
#' This is useful for dealing with floating-point arithmetic issues.
#'
#' @param tolerance A non-negative numeric value specifying the tolerance level.
#' @return None; this function sets an option and does not return a value.
#' @export
#' @examples
#' set_tolerance(0.001)

set_tolerance <- function(tolerance = 0) {
  if (!is.numeric(tolerance) || tolerance < 0) {
    stop("Tolerance must be a non-negative numeric value.")
  }
  options(comparison_tolerance = tolerance)
  message("Tolerance set to ", tolerance)
}

get_tolerance <- function() {
  return(getOption("comparison_tolerance", default = 0))
}