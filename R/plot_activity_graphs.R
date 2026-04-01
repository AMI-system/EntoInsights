#' Generate graph illustrating activity (detections or species) over the deployment period
#'
#' @param dataframe Results dataframe containing classifications.
#' @param type Character string specifying what to plot.
#'    `"detections"` will show the number of detected individuals per night/day,
#'    `"species"` will show the number of unique species detected per night/day.
#'
#' @return ggplot graph of activity (detections or species) over the deployment period
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom scales pretty_breaks
#' @export
plot_activity_graph <- function(dataframe, type){

  if (!type %in% c("detections", "species")) {
    stop('`type` must be either "detections" or "species"')
  }

  # Step 1: define valid sessions based on data type
  if (!"crop_status" %in% names(dataframe)) {
    # Bird data → keep only sessions with >=10 files
    valid_sessions <- dataframe %>%
      dplyr::group_by(recording_session) %>%
      dplyr::summarise(n_files = dplyr::n_distinct(filepath), .groups = "drop") %>%
      dplyr::filter(n_files >= 10) %>%
      dplyr::select(recording_session)
  } else {
    # Moth data → keep all sessions
    valid_sessions <- dataframe %>%
      dplyr::distinct(recording_session)
  }

  # Step 2: filter dataframe to valid sessions
  dataframe <- dataframe %>%
    dplyr::filter(recording_session %in% valid_sessions$recording_session)

  # Step 3: for moths, remove "No detections" rows
  if ("crop_status" %in% names(dataframe)) {
    dataframe <- dataframe %>%
      dplyr::filter(crop_status != "No detections for this image.")
  }

  # Step 4: prepare plotting data
  all_nights <- valid_sessions %>%
    dplyr::mutate(recording_session = as.Date(recording_session))

  if (type == "detections"){

    detections_df <- dataframe %>%
      dplyr::group_by(recording_session) %>%
      dplyr::summarise(n_detections = dplyr::n(), .groups = "drop")

    detections_complete <- all_nights %>%
      dplyr::left_join(detections_df, by = "recording_session") %>%
      dplyr::mutate(n_detections = tidyr::replace_na(n_detections, 0))

    detections_plot <- ggplot(detections_complete, aes(x = recording_session, y = n_detections)) +
      geom_point(size=3) +
      geom_line(linewidth=1) +
      labs(x = "Recording session", y = "Number of detections") +
      scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 0.5),
            axis.text = element_text(size=18),
            axis.title.x = element_text(size=25, vjust = -3),
            axis.title.y = element_text(size=25, vjust = +3),
            plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "inches"))

    return(detections_plot)

  } else { # species plot

    species_df <- dataframe %>%
      dplyr::group_by(recording_session) %>%
      dplyr::summarise(n_species = dplyr::n_distinct(top_species_prediction), .groups = "drop")

    species_complete <- all_nights %>%
      dplyr::left_join(species_df, by = "recording_session") %>%
      dplyr::mutate(n_species = tidyr::replace_na(n_species, 0))

    species_plot <- ggplot(species_complete, aes(x = recording_session, y = n_species)) +
      geom_point(size=3) +
      geom_line(linewidth=1) +
      labs(x = "Recording session", y = "Number of species") +
      scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 0.5),
            axis.text = element_text(size=18),
            axis.title.x = element_text(size=25, vjust = -3),
            axis.title.y = element_text(size=25, vjust = +3),
            plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "inches"))

    return(species_plot)

  }

}
