























transform_variables <- function(df, transformations) {
  for (var in names(transformations)) {
    if (var %in% names(df)) {
      # Applying the transformation
      transform_function <- transformations[[var]]
      df[[var]] <- transform_function(df[[var]])
    } else {
      warning(paste("Variable", var, "not found in the dataset."))
    }
  }
  df
}







