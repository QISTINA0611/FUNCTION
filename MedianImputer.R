# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

detect_missing_values <- function(data) {
  missing_counts <- numeric(ncol(data))
  names(missing_counts) <- colnames(data)

  for(i in 1:ncol(data)) {
    missing_counts[i] <- sum(is.na(data[[i]]))
  }

  missing_counts <- missing_counts[missing_counts > 0]

  return(missing_counts)
}

sample_data <- data.frame(
  Age = c(45,20,23,24,NA,19),
  Weight = c(81,NA,55,60,42,78),
  Height = c(155,178,181,162, NA, 144)
)

detect_missing_values(sample_data)


median_impute <- function(x) {
  x[is.na(x)] <- median (x,na.rm = TRUE)
  return(x)
}

sample_data <- sapply(sample_data, median_impute)
print(sample_data)
