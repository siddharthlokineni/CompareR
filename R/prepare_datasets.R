#' Prepare Datasets for Comparison
#'
#' Prepares datasets for comparison by performing steps like filtering, sorting, 
#' or handling missing values based on predefined criteria.
#'
#' @param df1 First dataset to be prepared.
#' @param df2 Second dataset to be prepared.
#' @param sort_columns Columns to sort the datasets by.
#' @param filter_criteria Criteria for filtering the datasets.
#' @return A list containing two prepared datasets.
#' @export
#' @examples
#' prepare_datasets(df1, df2, sort_columns = "variable", filter_criteria = "variable > 5")

prepare_datasets <- function(df1, df2, sort_columns = NULL, filter_criteria = NULL) {
  if (!is.null(sort_columns)) {
    if (all(sort_columns %in% names(df1)) && all(sort_columns %in% names(df2))) {
      df1 <- df1 %>% arrange(!!!syms(sort_columns))
      df2 <- df2 %>% arrange(!!!syms(sort_columns))
    } else {
      warning("Some sorting columns are not present in the datasets.")
    }
  }
  
  if (!is.null(filter_criteria)) {
    df1 <- df1 %>% filter(!!rlang::parse_expr(filter_criteria))
    df2 <- df2 %>% filter(!!rlang::parse_expr(filter_criteria))
  }
  
  list(df1 = df1, df2 = df2)
}

