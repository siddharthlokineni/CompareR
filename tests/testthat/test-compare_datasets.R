library(testthat)
library(CompareR) 

test_that("compare_datasets returns a character string", {
  df1 <- data.frame(x = 1:5, y = c("A", "B", "C", "D", "E"))
  df2 <- data.frame(x = 1:5, y = c("A", "B", "C", "D", "F"))
  
  comparison <- compare_datasets(df1, df2)
  expect_type(comparison, "character")
})

test_that("compare_datasets checks dimensions", {
  df1 <- data.frame(x = 1:5, y = c("A", "B", "C", "D", "E"))
  df2 <- data.frame(x = 1:4, y = c("A", "B", "C", "D"))
  
  comparison <- compare_datasets(df1, df2)
  expect_match(comparison, "Datasets have different dimensions.")
})

test_that("compare_datasets checks column names", {
  df1 <- data.frame(a = 1:5, b = c("A", "B", "C", "D", "E"))
  df2 <- data.frame(x = 1:5, y = c("A", "B", "C", "D", "E"))
  
  comparison <- compare_datasets(df1, df2)
  expect_match(comparison, "Common Columns:")
  expect_match(comparison, "Extra Columns in Dataset 1:")
  expect_match(comparison, "Extra Columns in Dataset 2:")
})

test_that("compare_datasets handles missing values correctly", {
  df1 <- data.frame(x = c(1, NA, 3), y = c("A", NA, "C"))
  df2 <- data.frame(x = c(1, 2, 3), y = c("A", "B", "C"))
  
  comparison <- compare_datasets(df1, df2)
  expect_match(comparison, "has missing values")
})


test_that("compare_datasets handles data type differences", {
  df1 <- data.frame(x = 1:5, y = c("A", "B", "C", "D", "E"))
  df2 <- data.frame(x = 1:5, y = as.factor(c("A", "B", "C", "D", "E")))
  
  comparison <- compare_datasets(df1, df2)
  expect_match(comparison, "has different data types in the two datasets.")
})

