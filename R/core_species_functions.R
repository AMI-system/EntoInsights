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
#' @details
#' This function is used internally by plotting functions to determine which species are most active.
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
#' @details
#' This is a helper function that calls \code{\link{summarise_species_activity}} internally.
#' Use this function when you want to quickly get the species most frequently detected over the deployment period.
#'
#' @seealso \code{\link{summarise_species_activity}}, \code{\link{get_top_species_nights}}
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
#' @details
#' This is a helper function that calls \code{\link{summarise_species_activity}} internally.
#' Use this function to find species that were consistently detected across multiple nights, regardless of total counts.
#'
#' @seealso \code{\link{summarise_species_activity}}, \code{\link{get_top_species_total}}
#'
#' @export
get_top_species_nights <- function(dataframe, n = 10) {

  summarise_species_activity(dataframe) %>%
    arrange(desc(nights_detected)) %>%
    slice_head(n = n)
}

#' Plot activity of top species over time using line-style plot
#'
#' @param dataframe Results dataframe.
#' @param type Character. Determines which species to plot: "detections", "nights", or "both". Default = "detections".
#'   - "detections": plot species with the highest total number of detections over the deployment.
#'   - "nights": plot species detected on the most nights (regardless of number of detections per night).
#'   - "both": plot species from both criteria. Species may appear in one or both groups. Line type indicates which ranking the species belongs to.
#' @param style "overlay" or "facet". Default = "overlay".
#' @param n Number of species to include (default 2). Maximum 8 species plotted.
#'
#' @return ggplot object.
#'
#' @details
#' This function internally calls \code{\link{summarise_species_activity}} to determine the total detections and nights detected per species.
#'
#' @seealso \code{\link{summarise_species_activity}}, \code{\link{get_top_species_total}}, \code{\link{get_top_species_nights}}
#'
#' @import ggplot2
#' @importFrom scales pretty_breaks
#' @export
plot_top_species_line <- function(dataframe,
                             type = c("detections", "nights", "both"),
                             style = c("overlay", "facet"),
                             n = 2) {

  type  <- match.arg(type)
  style <- match.arg(style)

  species_summary <- summarise_species_activity(dataframe)

  # ---- Enforce limit for non-both cases ----
  if (type != "both" && n > 8) {
    warning("Maximum of 8 species can be plotted. Showing top 8.")
    n <- 8
  }

  # ---- Select species ----
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

  } else {

    # Select up to n from each
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

    # Enforce total max of 8
    if (nrow(selected_species) > 8) {
      warning(
        paste0(
          "With type = 'both' and n = ", n,
          ", up to ", 2*n, " species could be selected. ",
          "Plot is limited to a maximum of 8 species, ",
          "so the top 8 by total detections are shown."
        )
      )
      selected_species <- selected_species %>%
        arrange(desc(total_detections)) %>%
        slice_head(n = 8)
    }
  }

  # ---- Prepare plotting data ----
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

  # ---- Base plot ----
  p <- ggplot(
    plot_data,
    aes(
      x = recording_session,
      y = n_detections,
      colour = top_species_prediction
    )
  )

  # Overlay or facet
  if (style == "overlay") {

    if (type == "both") {
      p <- p + aes(linetype = rank_type)
    }

    p <- p +
      geom_line(linewidth = 0.5) +
      geom_point(size = 3)

  } else {

    if (type == "both") {
      p <- p + aes(linetype = rank_type)
    }

    p <- p +
      geom_line(linewidth = 0.5) +
      geom_point(size = 3) +
      facet_wrap(~ top_species_prediction, scales = "free_y")
  }

  # ---- Final styling ----
  p <- p +
    scale_colour_brewer(palette = "Dark2") +
    scale_y_continuous(breaks = pretty_breaks(n = 8)) +
    labs(
      x = "Recording night",
      y = "Number of detections",
      colour = "Species",
      linetype = if (type == "both") "Ranking" else NULL
    ) +
    theme_classic() +
    theme(
      axis.title = element_text(size = 16),
      axis.text  = element_text(size = 14),
      legend.title = element_text(size = 15),
      legend.text  = element_text(size = 13),
      strip.text = element_blank() # remove individual plot titles
    )

  # ---- Add borders only for faceted plots ----
  if (style == "facet") {
    p <- p + theme(
      panel.border = element_rect(colour = "black", fill = NA)
    )
  }

  return(p)
}

#' Plot activity of top species over time using bubble-style plot
#'
#' @param dataframe Results dataframe
#' @param type Character. Determines which species are plotted: "detections" or "nights" (default "detections")
#'   - "detections": plot species with the highest total number of detections over the deployment.
#'   - "nights": plot species detected on the most nights (regardless of how many detections per night).
#' @param n Number of species to include (default 3)
#'
#' @return ggplot object showing species activity over time
#'
#' @details
#' Bubble size always represents the number of detections per night.
#' This function internally calls \code{\link{summarise_species_activity}} to select the top species for plotting.
#'
#' @seealso \code{\link{summarise_species_activity}}, \code{\link{get_top_species_total}}, \code{\link{get_top_species_nights}}
#'
#' @import ggplot2
#' @export
plot_top_species_bubble <- function(dataframe,
                                type = c("detections", "nights"),
                                n = 3) {

  type <- match.arg(type)

  # ---- Enforce max number of species ----
  if (n > 8) {
    warning("Maximum of 8 species can be plotted (Dark2 palette limitation). Showing top 8.")
    n <- 8
  }

  # ---- Summarise species activity ----
  species_summary <- summarise_species_activity(dataframe)

  # ---- Select top species ----
  if (type == "detections") {
    selected_species <- species_summary %>%
      arrange(desc(total_detections)) %>%
      slice_head(n = n) %>%
      pull(top_species_prediction)
  } else {
    selected_species <- species_summary %>%
      arrange(desc(nights_detected)) %>%
      slice_head(n = n) %>%
      pull(top_species_prediction)
  }

  # ---- Prepare data ----
  bubble_data <- dataframe %>%
    filter(
      crop_status != "No detections for this image.",
      top_species_prediction %in% selected_species
    ) %>%
    group_by(recording_session, top_species_prediction) %>%
    summarise(n_detections = n(), .groups = "drop")

  bubble_data$recording_session <- as.Date(bubble_data$recording_session)

  # Ensure species ordered from top to bottom
  bubble_data$top_species_prediction <-
    factor(bubble_data$top_species_prediction,
           levels = rev(selected_species))

  # ---- Bubble plot ----
  p <- ggplot(
    bubble_data,
    aes(
      x = recording_session,
      y = top_species_prediction,
      size = n_detections,
      colour = top_species_prediction
    )
  ) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(3, 12)) +
    scale_colour_brewer(palette = "Dark2", guide = "none") +  # remove legend
    labs(
      x = "Recording night",
      y = "Species",
      size = "Detections"
    ) +
    theme_classic() +
    theme(
      axis.title = element_text(size = 16),
      axis.text  = element_text(size = 14),
      legend.title = element_text(size = 15),
      legend.text  = element_text(size = 13)
    )

  return(p)
}
