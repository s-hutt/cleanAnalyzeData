# Loading the packages "usethis", "devtools" and "roxygen2":

library(usethis)
library(devtools)
library(roxygen2)

#' Clean Data Function
#'
#' This function cleans the input data by removing missing values, removing duplicates, and converting column names to lowercase.
#'
#' @param data A data frame containing the data to be cleaned.
#' @return A cleaned data frame.
#' @examples
#' data <- data.frame(A = c(1, 2, NA, 4, 4), B = c("a", "b", "c", "d", "d"))
#' clean_data(data)
#' @export
clean_data <- function(data) {
  data <- na.omit(data)
  data <- data[!duplicated(data), ]
  colnames(data) <- tolower(colnames(data))
  return(data)
}

#' Summarize Data Function
#'
#' This function summarizes the input data by calculating the mean, median, standard deviation, and count for each numeric column.
#'
#' @param data A data frame containing the data to be summarized.
#' @return A data frame with summary statistics for each numeric column.
#' @examples
#' data <- data.frame(A = c(1, 2, 3, 4), B = c(2, 3, 4, 5))
#' summarize_data(data)
#' @export
summarize_data <- function(data) {
  summary <- data.frame(
    Column = colnames(data),
    Mean = sapply(data, mean, na.rm = TRUE),
    Median = sapply(data, median, na.rm = TRUE),
    SD = sapply(data, sd, na.rm = TRUE),
    N = sapply(data, function(x) sum(!is.na(x)))
  )
  return(summary)
}
