#' Converts the data types of specified variables in a dataset. 
#'
#' This is useful for ensuring that variables are in the correct format 
#' for analysis or comparison.
#'
#' @param df A data frame containing the variables to be converted.
#' @param conversions A named list where names correspond to variable names 
#' in the dataset, and values are the desired data types (e.g., 'numeric', 'factor').
#' @return A data frame with converted variable types.
#' @export
#' @examples
#' convert_data_types(df, conversions = list(var1 = 'numeric', var2 = 'factor'))

convert_data_types <- function(df, conversions) {
  for (var_name in names(conversions)) {
    target_type <- conversions[[var_name]]
    
    # Check if the variable exists in the data frame
    if (var_name %in% names(df)) {
      current_type <- class(df[[var_name]])
      
      # Only convert if the current type doesn't match the target type
      if (current_type != target_type) {
        df[[var_name]] <- as(df[[var_name]], target_type)
      }
    }
  }
  df
}
