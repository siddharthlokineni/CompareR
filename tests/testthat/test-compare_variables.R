library(testthat)
library(CompareR) 

test_that("compare_variables correctly identifies common and extra columns", {
  df1 <- data.frame(a = 1:5, b = LETTERS[1:5])
  df2 <- data.frame(a = 6:10, c = letters[1:5])
  
  comparison <- compare_variables(df1, df2)
  
  expect_equal(comparison$discrepancies, length(comparison$details$extra_in_df1) + length(comparison$details$extra_in_df2))
  expect_equal(comparison$details$common_columns, "a")
  expect_equal(comparison$details$extra_in_df1, "b")
  expect_equal(comparison$details$extra_in_df2, "c")
})

test_that("compare_variables correctly compares data types", {
  df1 <- data.frame(x = 1:5, y = LETTERS[1:5])
  df2 <- data.frame(x = 6:10, y = as.factor(LETTERS[1:5]))
  
  comparison <- compare_variables(df1, df2)
  
  data_type_comparison <- comparison$details$data_type_comparisons
  expect_equal(data_type_comparison[[1]]$column, "x")
  expect_equal(data_type_comparison[[1]]$type_df1, "integer")
  expect_equal(data_type_comparison[[1]]$type_df2, "integer")
  
  expect_equal(data_type_comparison[[2]]$column, "y")
  expect_equal(data_type_comparison[[2]]$type_df1, "character")
  expect_equal(data_type_comparison[[2]]$type_df2, "factor")
})

# Test for variable names/labels
test_that("compare_variables correctly identifies variable names/labels", {
  df1 <- data.frame(x = 1:5, y = LETTERS[1:5])
  names(df1) <- c("Var1", "Var2")
  df2 <- data.frame(x = 6:10, z = letters[1:5])
  names(df2) <- c("Var1", "Var3")
  
  comparison <- compare_variables(df1, df2)
  
  expect_equal(comparison$details$common_columns, "Var1")
  expect_equal(comparison$details$extra_in_df1, "Var2")
  expect_equal(comparison$details$extra_in_df2, "Var3")
})

# Test for data types, specifically dates and date formats
test_that("compare_variables correctly compares data types including dates", {
  df1 <- data.frame(date_col = as.Date(c('2020-01-01', '2020-01-02')), x = 1:2)
  df2 <- data.frame(date_col = as.Date(c('2020-01-01', '2020-01-02'), format = "%Y-%m-%d"), y = 1:2)
  
  comparison <- compare_variables(df1, df2)
  
  data_type_comparison <- comparison$details$data_type_comparisons
  expect_equal(data_type_comparison[[1]]$column, "date_col")
  expect_equal(data_type_comparison[[1]]$type_df1, "Date")
  expect_equal(data_type_comparison[[1]]$type_df2, "Date")
})


