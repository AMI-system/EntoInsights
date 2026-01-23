#' Wrangle the data into a standardised format
#'
#' This function takes a concatenated csv file for a deployment and
#' runs a series of checks and data wrangling steps to format the data in
#' a standardised way suitable for the downstream analysis pipeline.
#' Currently just set-up for moth results (AMI/LepiSense)
#'
#' @param results_file A string specifying the name of the file containing the concatenated csv results files
#' @param taxa A string specifying the name of the taxa being analysed - e.g., moth, bird, bat
#' @param filepath_colname A string specifying the name of the column containing filepaths
#' @param sitename_colname A string specifying the name of the column containing site names
#' @param latitude_colname A string specifying the name of the column containing latitudes
#' @param longitude_colname A string specifying the name of the column containing longitude
#' @param topspeciespred_colname A string specifying the name of the column containing top species predictions
#' @param speciesconf_colname A string specifying the name of the column containing species confidence scores
#'
#' @return A data.frame containing the wrangled results.
#' @export
#'
wrangle_results <- function(results_file, taxa, filepath_colname,
                            sitename_colname, latitude_colname, longitude_colname,
                            topspeciespred_colname, speciesconf_colname){

  # For now, need to check this is for moths (stop if not)
  if (taxa != "moth") {
    stop("Can only wrangle moth results currently. Need to adapt for bird and bat")
  }

  # Check file exists
  if (!file.exists(results_file)) {
    stop("File does not exist: ", results_file)
  }

  results <- read.csv(results_file)

  ### Checks and wrangling

  # Check all columns necessary are present:
  required_cols <- c(
    filepath_colname,
    sitename_colname,
    latitude_colname,
    longitude_colname,
    topspeciespred_colname,
    speciesconf_colname
  )

  missing_cols <- setdiff(required_cols, names(results))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # 1. Update all the necessary column names to our standardised ones
  names(results)[names(results) == filepath_colname] <- "filepath"
  names(results)[names(results) == sitename_colname] <- "site_name"
  names(results)[names(results) == latitude_colname] <- "latitude"
  names(results)[names(results) == longitude_colname] <- "longitude"
  names(results)[names(results) == topspeciespred_colname] <- "top_species_prediction"
  names(results)[names(results) == speciesconf_colname] <- "species_confidence"

  # 2.	Get date column - either update date colname OR create new column containing date
  # 3.	Get time column - create new column from datetime column
  # 4.	Get sampling_day_or_night column - create new column from datetime column

  # 	Join lookup tables (site → lat/lon) if needed

  # 	Correct variable types e.g., Coerces confidence to numeric

  # 	Flags missing lat/lon/site issues early

  # 	Flag nights where too few or too many images – indication something went wrong on those nights.

  return(results)

}
