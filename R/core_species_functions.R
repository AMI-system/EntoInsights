#' @import dplyr
#' @importFrom magrittr %>%
NULL # <- this line tells roxygen the imports are not attached to any function

#' Summarise species activity over the deployment period
#'
#' Calculates total detections and total nights detected (raw count).
#'
#' @param dataframe Results dataframe containing classifications.
#'
#' @return A dataframe with one row per species and summary activity metrics.
#'
#' @export
summarise_species_activity <- function(dataframe) {

  species_summary <- dataframe %>%
    filter(
      crop_status != "No detections for this image.",
      top_species_prediction != "",
      !is.na(top_species_prediction)
    ) %>%
    group_by(top_species_prediction) %>%
    summarise(
      total_detections = n(),
      nights_detected  = n_distinct(recording_session),
      .groups = "drop"
    ) %>%
    arrange(desc(total_detections))

  return(species_summary)

}

#' Get top species ranked by total detections
#'
#' @param dataframe Results dataframe.
#' @param n Number of species to return (default 10).
#'
#' @return Dataframe of top species ranked by total detections.
#'
#' @export
get_top_species_total <- function(dataframe, n = 10) {

  summarise_species_activity(dataframe) %>%
    arrange(desc(total_detections)) %>%
    slice_head(n = n)
}

#' Get top species ranked by nights detected
#'
#' @param dataframe Results dataframe.
#' @param n Number of species to return (default 10).
#'
#' @return Dataframe of top species ranked by nights detected.
#'
#' @export
get_top_species_nights <- function(dataframe, n = 10) {

  summarise_species_activity(dataframe) %>%
    arrange(desc(nights_detected)) %>%
    slice_head(n = n)
}

#' Plot activity of top species over time
#'
#' @param dataframe Results dataframe.
#' @param type "detections", "nights", or "both".
#' @param n Number of species to include (default 3).
#'
#' @return ggplot object showing species activity over time.
#'
#' @import ggplot2
#' @export
plot_top_species <- function(dataframe,
                             type = c("detections", "nights", "both"),
                             n = 3) {

  type <- match.arg(type)

  species_summary <- summarise_species_activity(dataframe)

  # Determine which species to plot
  if (type == "detections") {

    selected_species <- species_summary %>%
      arrange(desc(total_detections)) %>%
      slice_head(n = n) %>%
      mutate(rank_type = "Top by detections")

  } else if (type == "nights") {

    selected_species <- species_summary %>%
      arrange(desc(nights_detected)) %>%
      slice_head(n = n) %>%
      mutate(rank_type = "Top by nights")

  } else {  # both

    top_det <- species_summary %>%
      arrange(desc(total_detections)) %>%
      slice_head(n = n) %>%
      mutate(rank_type = "Top by detections")

    top_nights <- species_summary %>%
      arrange(desc(nights_detected)) %>%
      slice_head(n = n) %>%
      mutate(rank_type = "Top by nights")

    selected_species <- bind_rows(top_det, top_nights) %>%
      distinct(top_species_prediction, .keep_all = TRUE)
  }

  # Filter original data for selected species
  plot_data <- dataframe %>%
    filter(
      crop_status != "No detections for this image.",
      top_species_prediction %in% selected_species$top_species_prediction
    ) %>%
    group_by(recording_session, top_species_prediction) %>%
    summarise(n_detections = n(), .groups = "drop") %>%
    left_join(
      selected_species %>%
        select(top_species_prediction, rank_type),
      by = "top_species_prediction"
    )

  plot_data$recording_session <- as.Date(plot_data$recording_session)

  p <- ggplot(plot_data,
              aes(x = recording_session,
                  y = n_detections,
                  colour = top_species_prediction,
                  linetype = rank_type)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      x = "Night",
      y = "Number of detections",
      colour = "Species",
      linetype = "Ranking"
    ) +
    theme_classic()

  return(p)
}


