#' Calculate number of operational nights (/days)
#'
#' @param dataframe Results dataframe
#'
#' @return A number for how many nights the system collected data during the deployment period
#' @export
#'
calculate_device_operation <- function(dataframe){

  operational_nights_df <- dataframe %>%
    distinct(deployment_id, recording_session)

  operational_nights <- nrow(operational_nights_df)

  return(operational_nights)

}

#' Create a figure showing technology operational period and how many files per night
#'
#' @param dataframe Results dataframe
#'
#' @return A ggplot barplot of nightly file counts e.g., number of images
#' @export
#'
plot_device_operation_barplot <- function(dataframe){

  # Count number of files per night per deployment
  nightly_counts <- dataframe %>%
    group_by(site_name, recording_session) %>%
    summarise(n_files = n_distinct(filepath), .groups = "drop")

  number_operational_nights <- calculate_device_operation(dataframe)

  # Plot
  barplot <- ggplot(nightly_counts, aes(x = recording_session, y = n_files)) +
    geom_col(fill = "steelblue") +
    labs(
      title = sprintf("Total recording nights = %s", number_operational_nights),
      x = "Date",
      y = "Number of Files"
    ) +
    scale_x_date(
      date_breaks = "2 weeks",
      date_labels = "%d %b"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, size = 16),
      axis.title.x = element_text(size = 18),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 18),
      plot.title = element_text(size = 20, hjust = 0.5)
    )

  return(barplot)

}

#' Create a 2nd option for a figure showing technology operational period (simpler but less detailed)
#'
#' @param dataframe Results dataframe
#'
#' @return A ggplot tileplot of nightly file counts e.g., number of images
#' @export
#'
plot_device_operation_tileplot <- function(dataframe){

  operational_nights_df <- dataframe %>%
    distinct(site_name, recording_session)

  tileplot <- ggplot(operational_nights_df, aes(x = recording_session, y = site_name)) +
    geom_tile(width = 1, height = 0.9, fill = "steelblue") +
    scale_x_date(
      limits = c(
        floor_date(min(ymd(operational_nights_df$recording_session)), "month"),
        ceiling_date(max(ymd(operational_nights_df$recording_session)), "month")
      ),
      date_breaks = "1 month",
      date_labels = "%b"
    ) +
    labs(x = "Date", y = "") +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size = 18),
          axis.title.x = element_text(size = 20)
    )

  return(tileplot)

}
