#' Wrangle the data into a standardised format
#'
#' This function takes a concatenated csv file for a deployment and
#' runs a series of checks and data wrangling steps to format the data in
#' a standardised way suitable for the downstream analysis pipeline.
#' Currently set-up for moth results (AMI/LepiSense) and bird results outputted by BirdNET
#'
#' @param results_file A string specifying the name of the file containing the concatenated csv results files
#' @param taxa A string specifying the name of the taxa being analysed - e.g., moth, bird, bat
#' @param source Optional. Provide model/source when selected taxa is 'bird' (currently must be "birdnet")
#' @param filepath_colname A string specifying the name of the column containing filepaths
#' @param sitename_colname A string specifying the name of the column containing site names
#' @param latitude_colname A string specifying the name of the column containing latitudes
#' @param longitude_colname A string specifying the name of the column containing longitudes
#' @param topspeciespred_colname A string specifying the name of the column containing top species predictions
#' @param speciesconf_colname A string specifying the name of the column containing species confidence scores
#'
#' @return A data.frame containing the wrangled results.
#' @export
wrangle_results <- function(results_file, taxa = "moth", source = NULL, filepath_colname = "image_path",
                            sitename_colname = "deployment_name", latitude_colname = "latitude", longitude_colname = "longitude",
                            topspeciespred_colname = "top_1_species", speciesconf_colname = "top_1_confidence", ...){

  # -----------------------
  # Input checks
  # -----------------------

  # Check this is for moths or birds (stop if not)
  if (!taxa %in% c("moth", "bird")) {
    stop("taxa must be 'moth' or 'bird'. 'bat' will be added later")
  }
  # If moth, source must be NULL
  if (taxa == "moth" && !is.null(source)) {
    stop("source must be NULL when taxa = 'moth'")
  }
  # If birds, must be birdnet currently
  if (taxa == "bird" && (is.null(source) || source != "birdnet")) {
    stop("Currently only 'birdnet' source is supported for birds")
  }

  # # For testing
  # results_file <- "C:/Users/graski/OneDrive - UKCEH/part-time_PhD/AgZero+/analysis/agzero_ami_2024_data/east_hendred/combined_results.csv"
  # results_file <- "C:/Users/graski/OneDrive - UKCEH/part-time_PhD/AgZero+/analysis/agzero_birdnet_2024_data/east_hendred/test_results.csv"


  # -----------------------
  # Load data
  # -----------------------

  # Load results
  if (is.character(results_file)) {
    if (!file.exists(results_file)) {
      stop("File does not exist: ", results_file)
    }
    results <- read.csv(results_file)

  } else if (is.data.frame(results_file)) {
    results <- results_file

  } else {
    stop("results_file must be a file path or a data frame")
  }

  # -----------------------
  # Shared wrangling steps (all taxa)
  # -----------------------

  # # For testing
  # Moths
  # filepath_colname <- "image_path"
  # sitename_colname <- "deployment_name"
  # latitude_colname <- "latitude"
  # longitude_colname <- "longitude"
  # topspeciespred_colname <- "top_1_species"
  # speciesconf_colname <- "top_1_confidence"
  # Birds birdnet
  # results_file <- results_file
  # filepath_colname <- "path"
  # sitename_colname <- "site"
  # latitude_colname <- "lat"
  # longitude_colname <- "lon"
  # topspeciespred_colname <- "scientific_name"
  # speciesconf_colname <- "confidence"
  # results$site <- basename(dirname(results_file))
  # results$lat <- 51.58972
  # results$lon <- -1.317683

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

  # Update all the necessary column names to our standardised ones
  names(results)[names(results) == filepath_colname] <- "filepath"
  names(results)[names(results) == sitename_colname] <- "site_name"
  names(results)[names(results) == latitude_colname] <- "latitude"
  names(results)[names(results) == longitude_colname] <- "longitude"
  names(results)[names(results) == topspeciespred_colname] <- "top_species_prediction"
  names(results)[names(results) == speciesconf_colname] <- "species_confidence"

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

  # IF needed: Correct variable types e.g., Coerces confidence to numeric (OK for moths and birdnet)
  # results$species_confidence <- as.numeric(results$species_confidence)

  # -----------------------
  # Taxa/source-specific wrangling
  # -----------------------

  results <- switch(
    taxa,

    # --- Moth-specific wrangling ---
    moth = wrangle_moth(results, ...), # call moth helper, forward extra arguments

    # --- Bird-specific wrangling ---
    bird = {
      results <- switch(
        source,

        # BirdNET processor
        birdnet = {
          results <- wrangle_birdnet(results, ...) # call birdnet helper, forward extra arguments
          results
        },

        # Placeholder for future bird sources
        stop("Unsupported bird source: ", source)
      )
      results
    }
  )

  return(results)

}

# ============================
# Internal helper: wrangle_moth
# ============================
#' Internal helper: wrangle moth results
#' @keywords internal
wrangle_moth <- function(results, ...) {

  # Get date column - either update date colname OR create new column containing date
  ## For moths, the date and time the image was taken is in image_datetime column
  ## Make image_datetime column a datetime object
  results$image_datetime <- as.POSIXct(results$image_datetime, format = "%Y-%m-%d %H:%M:%S", tz="GMT")
  # Extract the date component
  results$date <- as.Date(results$image_datetime)

  # Get time column - create new column from datetime column
  # Extract time component
  results$time <- format(results$image_datetime, format = "%H:%M:%S")

  # Get sampling_day_or_night (recording_session)
  ## For moths, this is now in the outputted results file under the column "recording_session"
  ## Make recording_session column a datetime object
  results$recording_session <- as.POSIXct(results$recording_session, format = "%Y-%m-%d", tz="GMT")

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

  results
}

# ============================
# Internal helper: wrangle_birdnet
# ============================
#' Internal helper: wrangle BirdNET bird results
#' @keywords internal
wrangle_birdnet <- function(results, ...) { # ... means pass along all extra arguments received by the main function to this helper without listing them explicitly

  # Get date column - either update date colname OR create new column containing date
  ## For birds, the date and time the image was taken is in column 'filename'
  ## Make image_datetime column a datetime object
  # Remove the .wav extension and parse datetime
  results$recording_datetime <- as.POSIXct(
    substr(results$filename, 1, 15),        # take the first 15 characters: "20240604_212002"
    format = "%Y%m%d_%H%M%S"                # tell R the format of the string
  )

  # Extract the date component
  results$date <- as.Date(results$recording_datetime)

  # Extract time component
  results$time <- format(results$recording_datetime, format = "%H:%M:%S")

  # Get sampling_day_or_night (recording_datetime) column
  ## For birds, the recording schedule is midday one day to midday the next.
  # So anything recorded between midday and 23:59 will have a recording_session matching the date
  # And anything recorded between midnight and 11:59 will have a recording session of date minus 1 day
  results$recording_session <- as.Date(
    ifelse(
      results$time < "12:00:00",  # midnight → 11:59 (did the recording happen before midday?)
      results$date - 1,  # if yes, belongs to previous day’s session
      results$date       # if no, i.e., 12:00 → 23:59 keeps same date
    )
  )

  ## Make recording_session column a datetime object
  results$recording_session <- as.POSIXct(results$recording_session, format = "%Y-%m-%d", tz="GMT")

  results
}
