#' Compare Datasets by Group
#'
#' Performs dataset comparisons based on specified grouping variables.
#' Useful for BY-group analysis in datasets.
#'
#' @param df1 First dataset for comparison.
#' @param df2 Second dataset for comparison.
#' @param group_vars Variables used for grouping the data.
#' @return A list of comparison results for each group.
#' @export
#' @examples
#' compare_by_group(df1, df2, group_vars = c("group_var1", "group_var2"))

compare_by_group <- function(df1, df2, group_vars) {
  if (!all(group_vars %in% names(df1)) || !all(group_vars %in% names(df2))) {
    stop("Grouping variables must be present in both datasets.")
  }
  
  # Splitting datasets by groups
  df1_split <- split(df1, df1[, group_vars, drop = FALSE])
  df2_split <- split(df2, df2[, group_vars, drop = FALSE])
  
  # Identifying unique groups in both datasets
  all_groups <- union(names(df1_split), names(df2_split))
  
  # Comparing each group
  results <- lapply(all_groups, function(group) {
    group_df1 <- df1_split[[group]] %>% dplyr::bind_rows()
    group_df2 <- df2_split[[group]] %>% dplyr::bind_rows()
    compare_datasets(group_df1, group_df2)
  })
  
  names(results) <- all_groups
  return(results)
}