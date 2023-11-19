#' Compare Two Datasets
#'
#' This function performs a high-level comparison of two datasets.
#' It checks for basic compatibility in terms of dimensions and variable names, 
#' and then performs detailed comparisons of variables and observations.
#'
#' @param df1 A data frame representing the first dataset.
#' @param df2 A data frame representing the second dataset.
#' @return A list containing the results of the comparison.
#' @export
#' @examples
#' compare_datasets(df1, df2)
compare_datasets <- function(df1, df2) {
  if (is.null(df1) || is.null(df2)) {
    return("One or both datasets are null.")
  }
  
  dimensions_equal <- all(dim(df1) == dim(df2))
  dimension_message <- if (dimensions_equal) {
    "Both datasets have the same dimensions."
  } else {
    "Datasets have different dimensions."
  }
  
  common_cols <- intersect(names(df1), names(df2))
  extra_df1 <- setdiff(names(df1), names(df2))
  extra_df2 <- setdiff(names(df2), names(df1))
  
  col_name_message <- if (length(extra_df1) == 0 && length(extra_df2) == 0) {
    "Both datasets have the same column names."
  } else {
    paste(
      "Common Columns:", paste(common_cols, collapse = ", "),
      "\nExtra Columns in Dataset 1:", paste(extra_df1, collapse = ", "),
      "\nExtra Columns in Dataset 2:", paste(extra_df2, collapse = ", ")
    )
  }
  
  data_type_comparison <- sapply(common_cols, function(col) {
    type_df1 <- class(df1[[col]])
    type_df2 <- class(df2[[col]])
    
    if (type_df1 != type_df2) {
      return(paste("Column '", col, "' has different data types in the two datasets."))
    } else {
      return(NULL)
    }
  })
  
  # New section to check for missing values
  missing_values_comparison <- sapply(common_cols, function(col) {
    na_df1 <- sum(is.na(df1[[col]]))
    na_df2 <- sum(is.na(df2[[col]]))
    
    if (na_df1 > 0 || na_df2 > 0) {
      return(paste("Column '", col, "' has missing values. Dataset1:", na_df1, "missing. Dataset2:", na_df2, "missing."))
    } else {
      return(NULL)
    }
  })
  
  result_messages <- c(
    dimension_message,
    col_name_message,
    data_type_comparison[!sapply(data_type_comparison, is.null)],
    missing_values_comparison[!sapply(missing_values_comparison, is.null)]
  )
  result_string <- paste(result_messages, collapse = "\n")
  
  return(result_string)
}
