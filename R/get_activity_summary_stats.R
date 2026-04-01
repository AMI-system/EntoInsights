#' Generate summary stats (mean, min, max) for the number of detections and species per day/night over the deployment.
#'
#' @param dataframe Results dataframe containing classifications.
#' @param detections Logical. If TRUE (default), return summary stats on detections (mean, min, max number of detections per day/night).
#' @param species Logical. If TRUE (default), return summary stats on species (mean, min, max number of species per day/night).
#'
#' @return List containing the specified summary statistics
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na
#' @export
get_activity_summary <- function(dataframe, detections = TRUE, species = TRUE){

  summary_stats <- list()

  session_counts <- dataframe %>%
    dplyr::group_by(recording_session) %>%
    dplyr::summarise(
      n_records = dplyr::n_distinct(filepath),
      .groups = "drop"
    )

  valid_sessions <- session_counts %>%
    dplyr::filter(n_records >= 10) %>%
    dplyr::select(recording_session)

  # Keep only valid sessions
  dataframe <- dataframe %>%
    dplyr::filter(recording_session %in% valid_sessions$recording_session)

  if ("crop_status" %in% names(dataframe)) {
    # Moth logic
    dataframe <- dataframe %>%
      dplyr::filter(crop_status != "No detections for this image.")
  } else {
    # Bird (or generic) logic → assume all rows are detections
    dataframe <- dataframe
  }

  if (detections){

    detections_df <- dataframe %>%
      dplyr::group_by(recording_session) %>%
      dplyr::summarise(n_detections = n(), .groups = "drop")

    detections_complete <- valid_sessions %>%
      dplyr::left_join(detections_df, by = "recording_session") %>%
      dplyr::mutate(n_detections = tidyr::replace_na(n_detections, 0))

    summary_stats$detections <- list(
      min_detections  = min(detections_complete$n_detections),
      mean_detections = round(mean(detections_complete$n_detections), 0),
      max_detections  = max(detections_complete$n_detections)
    )

  }

  if (species){

    species_df <- dataframe %>%
      dplyr::group_by(recording_session) %>%
      dplyr::summarise(n_species = dplyr::n_distinct(top_species_prediction), .groups = "drop")

    species_complete <- valid_sessions %>%
      dplyr::left_join(species_df, by = "recording_session") %>%
      dplyr::mutate(n_species = tidyr::replace_na(n_species, 0))

    summary_stats$species <- list(
      min_species  = min(species_complete$n_species),
      mean_species = round(mean(species_complete$n_species), 0),
      max_species  = max(species_complete$n_species)
    )

  }

  return(summary_stats)

}
