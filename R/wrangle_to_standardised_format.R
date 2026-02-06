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
#' @param longitude_colname A string specifying the name of the column containing longitudes
#' @param topspeciespred_colname A string specifying the name of the column containing top species predictions
#' @param speciesconf_colname A string specifying the name of the column containing species confidence scores
#'
#' @return A data.frame containing the wrangled results.
#' @export
#'
wrangle_results <- function(results_file, taxa = "moth", filepath_colname = "image_path",
                            sitename_colname = "deployment_name", latitude_colname = "latitude", longitude_colname = "longitude",
                            topspeciespred_colname = "top_1_species", speciesconf_colname = "top_1_confidence"){

  # For now, need to check this is for moths (stop if not)
  if (taxa != "moth") {
    stop("Can only wrangle moth results currently. Need to adapt for bird and bat")
  }

  # # For testing
  # results_file <- "C:/Users/graski/OneDrive - UKCEH/part-time_PhD/AgZero+/analysis/agzero_ami_2024_data/east_hendred/combined_results.csv"

  # Check file exists
  if (!file.exists(results_file)) {
    stop("File does not exist: ", results_file)
  }

  results <- read.csv(results_file)

  ### Checks and wrangling

  # # For testing
  # filepath_colname <- "image_path"
  # sitename_colname <- "deployment_name"
  # latitude_colname <- "latitude"
  # longitude_colname <- "longitude"
  # topspeciespred_colname <- "top_1_species"
  # speciesconf_colname <- "top_1_confidence"

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
  ## For moths, the date and time the image was taken is in image_datetime column
  ## Make image_datetime column a datetime object
  results$image_datetime <- as.POSIXct(results$image_datetime, format = "%Y-%m-%d %H:%M:%S", tz="GMT")
  # Extract the date component
  results$date <- as.Date(results$image_datetime)

  # 3.	Get time column - create new column from datetime column
  # Extract time component
  results$time <- format(results$image_datetime, format = "%H:%M:%S")

  # 4.	Get sampling_day_or_night (image_datetime) column - potentially need to create new column from datetime column
  ## For moths, this is now in the outputted results file under the column "recording_session"
  ## Make image_datetime column a datetime object
  results$recording_session <- as.POSIXct(results$recording_session, format = "%Y-%m-%d", tz="GMT")

  # Join lookup tables (site → lat/lon) if needed - not needed for moths as lat/lon already in outputted results

  # Correct variable types e.g., Coerces confidence to numeric - all variable types seem appropriate for moths

  # Flags missing lat/lon/site issues early
  # Return warning if any rows have empty latitude, longitude, or site_name columns
  missing_info <- results[
    is.na(results$site_name) | results$site_name == "" |
      is.na(results$latitude)  |
      is.na(results$longitude),
  ]

  if (nrow(missing_info) > 0) {
    warning(
      sprintf(
        "Found %d rows with missing site_name and/or latitude/longitude",
        nrow(missing_info)
      )
    )
  }

  # Flag nights where too few or too many images – indication something went wrong on those nights.
  ## For moths, meant to be image every minute between sunrise and sunset. Average 10 hours night between April and Oct, so expect ~600 images a night.
  # Flag if below 100 images (based on unique filepaths per recording session) or above 1000
  recording_session_counts <- results %>%
    dplyr::group_by(site_name, recording_session) %>%
    dplyr::summarise(
      n_images = dplyr::n_distinct(filepath),
      .groups = "drop"
    )

  problem_recording_session <- recording_session_counts %>%
    dplyr::filter(n_images < 100 | n_images > 1000)

  if (nrow(problem_recording_session) > 0) {
    warning(
      sprintf(
        "Detected %d nights with anomalous image counts (<100 or >1000).",
        nrow(problem_recording_session)
      )
    )
  }

  return(results)

}
