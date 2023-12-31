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
    stop("One or both datasets are null.")
  }
  
  # Initialize a data frame to store comparison results
  comparison_results <- data.frame(
    Aspect = character(),
    Description = character(),
    stringsAsFactors = FALSE
  )
  
  # Check dimensions
  dimensions_equal <- all(dim(df1) == dim(df2))
  dimension_message <- if (dimensions_equal) {
    "Both datasets have the same dimensions."
  } else {
    "Datasets have different dimensions."
  }
  comparison_results <- rbind(comparison_results, data.frame(Aspect = "Dimensions", Description = dimension_message))
  
  # Column names comparison
  common_cols <- intersect(names(df1), names(df2))
  extra_df1 <- setdiff(names(df1), names(df2))
  extra_df2 <- setdiff(names(df2), names(df1))
  col_name_message <- paste(
    "Common Columns:", paste(common_cols, collapse = ", "),
    "\nExtra Columns in Dataset 1:", paste(extra_df1, collapse = ", "),
    "\nExtra Columns in Dataset 2:", paste(extra_df2, collapse = ", ")
  )
  comparison_results <- rbind(comparison_results, data.frame(Aspect = "Column Names", Description = col_name_message))
  
  # Data type comparison
  data_type_messages <- sapply(common_cols, function(col) {
    type_df1 <- class(df1[[col]])
    type_df2 <- class(df2[[col]])
    
    if (type_df1 != type_df2) {
      paste("Column '", col, "' has different data types in the two datasets. Types:", type_df1, "&", type_df2)
    } else {
      NULL
    }
  })
  
  # Missing values comparison
  missing_values_messages <- sapply(common_cols, function(col) {
    na_df1 <- sum(is.na(df1[[col]]))
    na_df2 <- sum(is.na(df2[[col]]))
    
    if (na_df1 > 0 || na_df2 > 0) {
      paste("Column '", col, "' has missing values. Dataset1:", na_df1, "missing. Dataset2:", na_df2, "missing.")
    } else {
      NULL
    }
  })
  
  # Add data type and missing values messages to results
  data_type_results <- data.frame(Aspect = rep("Data Types", length(data_type_messages)), Description = data_type_messages, stringsAsFactors = FALSE)
  missing_values_results <- data.frame(Aspect = rep("Missing Values", length(missing_values_messages)), Description = missing_values_messages, stringsAsFactors = FALSE)
  
  comparison_results <- rbind(comparison_results, data_type_results[!sapply(data_type_results$Description, is.null), ])
  comparison_results <- rbind(comparison_results, missing_values_results[!sapply(missing_values_results$Description, is.null), ])
  
  return(comparison_results)
}

