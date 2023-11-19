#' Transform Variables in a Dataset
#'
#' Applies specified transformations to variables in a dataset. 
#' Useful for standardizing data or converting variables to a consistent format or scale.
#'
#' @param df A data frame containing the variables to be transformed.
#' @param transformations A list of functions for transforming the variables.
#' The names of the list should correspond to the variable names in the dataset.
#' @return A data frame with transformed variables.
#' @export
#' @examples
#' transform_variables(df, list(var1 = as.numeric, var2 = as.factor))

transform_variables <- function(df, transformations) {
  for (var in names(transformations)) {
    if (var %in% names(df)) {
      # Applying the transformation
      transform_function <- transformations[[var]]
      df[[var]] <- transform_function(df[[var]])
    } else {
      warning(paste("Variable", var, "not found in the dataset."))
    }
  }
  df
}