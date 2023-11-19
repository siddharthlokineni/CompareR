#' Compare Observations of Two Datasets
#'
#' Performs row-wise comparison to identify differences in data values between two datasets.
#' Highlights rows with differences.
#'
#' @param df1 A data frame representing the first dataset.
#' @param df2 A data frame representing the second dataset.
#' @return A data frame showing the rows where the datasets differ.
#' @export
#' @examples
#' compare_observations(df1, df2)

compare_observations <- function(df1, df2) {
  # Ensure the data frames are of the same length
  if (nrow(df1) != nrow(df2)) {
    stop("The datasets have different numbers of rows.")
  }
  
  # Find common columns
  common_cols <- intersect(names(df1), names(df2))
  
  # Initialize a list to store row differences
  row_differences <- list()
  
  # Iterate through each common column
  for (col in common_cols) {
    differences <- which(df1[[col]] != df2[[col]])
    if (length(differences) > 0) {
      row_differences[[col]] <- data.frame(
        Row = differences,
        Value_in_df1 = df1[differences, col, drop = FALSE],
        Value_in_df2 = df2[differences, col, drop = FALSE]
      )
    }
  }
  
  # Combine all differences into a single data frame
  do.call(rbind, row_differences)
  
  # Count discrepancies for each variable
  discrepancy_counts <- sapply(row_differences, nrow)
  list(discrepancies = discrepancy_counts, details = row_differences)
  
}