#' Handle Missing Values in Dataset
#'
#' Applies specified method for handling missing values in the dataset. 
#' Options include excluding rows with missing values, replacing them, or flagging them.
#'
#' @param df A data frame with potential missing values.
#' @param method Method for handling missing values ('exclude', 'replace', 'mean', 'median', 'flag').
#' @param replace_with Optional; a value or named list to replace missing values with (used with 'replace' method).
#' @return A data frame after handling missing values.
#' @export
#' @examples
#' handle_missing_values(df, method = "exclude")

handle_missing_values <- function(df, method = "exclude", replace_with = NULL) {
  if (method == "exclude") {
    df <- na.omit(df)
  } else if (method == "replace") {
    if (is.null(replace_with)) {
      stop("Please specify a value to replace missing data with using 'replace_with' parameter.")
    }
    df <- replace_na(df, replace_with)
  } else if (method == "mean") {
    numeric_cols <- sapply(df, is.numeric)
    df[, numeric_cols] <- lapply(df[, numeric_cols, drop = FALSE], function(col) {
      replace(col, is.na(col), mean(col, na.rm = TRUE))
    })
  } else if (method == "median") {
    numeric_cols <- sapply(df, is.numeric)
    df[, numeric_cols] <- lapply(df[, numeric_cols, drop = FALSE], function(col) {
      replace(col, is.na(col), median(col, na.rm = TRUE))
    })
  } else if (method == "flag") {
    df$missing_flag <- apply(df, 1, function(x) any(is.na(x)))
  } else {
    stop("Invalid method specified. Choose from 'exclude', 'replace', 'mean', 'median', or 'flag'.")
  }
  
  df
}