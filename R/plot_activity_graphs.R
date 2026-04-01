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

  all_nights <- dataframe %>%
    dplyr::group_by(recording_session) %>%
    dplyr::summarise(
      n_records = dplyr::n_distinct(filepath),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_records >= 10) %>% # keep only sessions where at least 10 files were recorded
    dplyr::select(recording_session) %>%
    dplyr::mutate(recording_session = as.Date(recording_session))

  if ("crop_status" %in% names(dataframe)) {
    dataframe <- dataframe %>%
      dplyr::filter(crop_status != "No detections for this image.")
  }

  if (type == "detections"){

    detections_df <- dataframe %>%
      group_by(recording_session) %>%
      summarise(n_detections = n(), .groups = "drop")

    detections_complete <- all_nights %>%
      left_join(detections_df, by = "recording_session") %>%
      mutate(n_detections = tidyr::replace_na(n_detections, 0))

    detections_plot <- ggplot(detections_complete, aes(x = recording_session, y = n_detections)) +
      geom_point(size=3) +
      labs(x = "Recording session", y = "Number of detections") +
      geom_line(linewidth=1) +
      scale_x_date(
        date_breaks = "2 weeks",
        date_labels = "%d %b"
      ) +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 0.5),
            axis.text = element_text(size=18),
            axis.title.x = element_text(size=25, vjust = -3),
            axis.title.y = element_text(size=25, vjust = +3),
            plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "inches"))

    return(detections_plot)

  } else { # make species graph

    species_df <- dataframe %>%
      group_by(recording_session) %>%
      summarise(n_species = n_distinct(top_species_prediction), .groups = "drop")

    species_complete <- all_nights %>%
      left_join(species_df, by = "recording_session") %>%
      mutate(n_species = tidyr::replace_na(n_species, 0))

    species_plot <- ggplot(species_complete, aes(x = recording_session, y = n_species)) +
      geom_point(size=3) +
      labs(x = "Recording session", y = "Number of species") +
      geom_line(linewidth=1) +
      scale_x_date(
        date_breaks = "2 weeks",
        date_labels = "%d %b"
      ) +
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
