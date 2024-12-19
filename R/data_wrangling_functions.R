#' Keep only moths from a dataset
#'
#' This function filters rows in a dataset to keep only those with a binary label of "moth".
#'
#' @param data A data frame containing the dataset to filter.
#' @param label_column The name of the column containing the labels. Defaults to "binary_label".
#' @return A data frame containing only rows where the binary_label is "moth".
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @examples
#' keep_only_moths(data)
#' @export
keep_only_moths <- function(data, label_column = "binary_label") {

  # Check the binary_label column exists in the dataset
  if (!label_column %in% names(data)) {
    stop(paste("Column", label_column, "not found in the dataset."))
  }

  # Filter the data to remove all the non-moths, and keep just the moths
  data %>%
    filter(.data[[label_column]] == "moth")
}
