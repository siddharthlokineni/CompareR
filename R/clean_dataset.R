#' Clean Dataset
#'
#' Cleans the given dataset by handling missing values, removing duplicates, 
#' and potentially standardizing formats for selected variables.
#'
#' @param df A data frame to be cleaned.
#' @param variables Optional; a vector of variable names to specifically clean. 
#' If NULL, applies cleaning to all variables.
#' @param remove_duplicates Logical; whether to remove duplicate rows.
#' @param standardize_formats Logical; whether to standardize the formats (e.g., to lower case for character variables).
#' @return A cleaned data frame.
#' @export
#' @examples
#' clean_dataset(df, variables = c("var1", "var2"), remove_duplicates = TRUE, standardize_formats = TRUE)


clean_dataset <- function(df, variables = NULL, remove_duplicates = TRUE, standardize_formats = TRUE) {
  # If no specific variables are specified, apply to all columns
  if (is.null(variables)) {
    variables <- names(df)
  }
  
  for (var in variables) {
    if (var %in% names(df)) {
      if (remove_duplicates) {
        df[[var]] <- df[[var]][!duplicated(df[[var]]), , drop = FALSE]
      }
      
      if (standardize_formats) {
        # Apply standardization only to character columns
        if (is.character(df[[var]])) {
          df[[var]] <- tolower(df[[var]])
        }
        # Additional format standardizations can be added here
      }
    } else {
      warning(paste("Variable", var, "not found in the dataset."))
    }
  }
  
  df
}