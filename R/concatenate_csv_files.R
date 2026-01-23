#' Concatenate csv files
#'
#' This function concatenates all csv files in a given directory into 1 csv file
#' For example, the moth classifier pipeline returns one results CSV per day. We
#' want to combine all these together for analysis purposes.
#'
#' @param dir A string specifying the name of the directory containing the csvs to concatenate
#'
#' @return Location of where concatenated csv has been saved
#' @export
#'
concat_csvs <- function(dir){

  # Check directory exists
  if (!dir.exists(dir)) {
    stop("Directory does not exist: ", dir)
  }

  # List files
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)

  # Read all CSVs and store in a list
  data_list <- lapply(files, function(f) {
    read.csv(f)
  })

  # Combine into one data frame
  combined <- dplyr::bind_rows(data_list)

  # Save
  write.csv(combined, file.path(dir, "combined_results.csv"), row.names = FALSE)

  message("Saved to: ", file.path(dir, "combined_results.csv"))

  invisible(NULL)  # explicitly returns nothing
}
