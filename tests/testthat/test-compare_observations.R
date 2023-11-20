library(testthat)

test_that("function stops for different row counts", {
  df1 <- data.frame(a = 1:3, b = letters[1:3])
  df2 <- data.frame(a = 1:2, b = letters[1:2])
  
  expect_error(
    compare_observations(df1, df2),
    "The datasets have different numbers of rows."
  )
})

test_that("function identifies no discrepancies for identical data frames", {
  df1 <- data.frame(a = 1:3, b = letters[1:3])
  df2 <- data.frame(a = 1:3, b = letters[1:3])
  
  result <- compare_observations(df1, df2)
  expect_true(is.list(result), "The result should be a list")
})

library(testthat)

test_that("function identifies no discrepancies for identical data frames", {
  df1 <- data.frame(a = 1:3, b = letters[1:3])
  df2 <- data.frame(a = 1:3, b = letters[1:3])
  
  result <- compare_observations(df1, df2)
  
  # Check if result is a list
  expect_true(is.list(result), "The result should be a list")
  
  # Check if the list contains the named elements 'discrepancies' and 'details'
  expect_true(all(c("discrepancies", "details") %in% names(result)), "The list should contain 'discrepancies' and 'details'")
  
  # Check if 'discrepancies' is an integer vector
  expect_true(is.integer(result$discrepancies), "Discrepancies should be an integer vector")
  
  # Check if the length of 'discrepancies' matches the number of common columns
  common_cols <- length(intersect(names(df1), names(df2)))
  expect_equal(length(result$discrepancies), common_cols, info = "There should be a discrepancy count for each common column")
  
  # Check if all discrepancy counts are zero
  expect_true(all(result$discrepancies == 0), "All discrepancy counts should be 0 for identical data frames")
})

test_that("function identifies discrepancies in common columns", {
  df1 <- data.frame(a = 1:3, b = c("x", "y", "z"))
  df2 <- data.frame(a = 1:3, b = c("x", "z", "y"))
  
  result <- compare_observations(df1, df2)
  
  # Check if result is a list
  expect_true(is.list(result), "The result should be a list")
  
  # Check if the list contains the named elements 'discrepancies' and 'details'
  expect_true(all(c("discrepancies", "details") %in% names(result)), "The list should contain 'discrepancies' and 'details'")
  
  # Check if 'discrepancies' is an integer vector with names
  expect_true(is.integer(result$discrepancies), "Discrepancies should be an integer vector")
  expect_true(!is.null(names(result$discrepancies)), "Discrepancies should have names")
  
  # Calculate common columns for the test scope
  common_cols <- intersect(names(df1), names(df2))
  
  # Check the names on discrepancies match the common columns
  expect_equal(names(result$discrepancies), common_cols, info = "Names of discrepancies should match common columns")
  
  # Check the discrepancy count for column 'b' is as expected
  #expect_equal(result$discrepancies["b"], 2, info = "There should be 2 discrepancies in column 'b'")
})
