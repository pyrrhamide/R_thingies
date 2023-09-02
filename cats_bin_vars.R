# pour mon usage - ChatGPT!!! one-hot encoding
df <- data.frame(category = c("A", "B", "C", "A", "C"))

bin <- model.matrix(~ category - 1, data = df)

cbind(df, bin)




# Create a sample data frame with categorical and numeric variables
df <- data.frame(
  category1 = c("A", "B", "C", "A", "C"),
  category2 = c("X", "Y", "X", "Y", "Z"),
  numeric_var = c(1, 2, 3, 4, 5),
  stringsAsFactors = FALSE
)

# Identify categorical variables
categorical_vars <- sapply(df, function(x) is.character(x) || is.factor(x))

# Use lapply to apply model.matrix to each categorical variable
binary_vars_list <- lapply(df[, categorical_vars], function(x) {
  if (is.character(x)) {
    x <- factor(x)
  }
  model.matrix(~ x - 1, data = data.frame(x))
})

# Combine binary variables into a single data frame
binary_vars <- do.call(cbind, binary_vars_list)

# Add binary variables to the original data frame
df <- cbind(df, binary_vars)

# Print the resulting data frame
df
