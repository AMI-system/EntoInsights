#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @import lubridate
NULL  # <- this line tells roxygen the imports are not attached to any function

#' Calculate number of operational nights (/days)
#'
#' @param dataframe Results dataframe
#'
#' @return A number for how many nights the system collected data during the deployment period
#' @export
#'
calculate_device_operation <- function(dataframe){

  operational_sessions_df <- dataframe %>%
    dplyr::group_by(deployment_id, recording_session) %>%
    dplyr::summarise(
      n_records = dplyr::n_distinct(filepath),
      .groups = "drop"
    )

  # Keep only sessions with >10 records
  valid_sessions <- operational_sessions_df %>%
    dplyr::filter(n_records >= 10)

  n_valid_sessions <- nrow(valid_sessions)

  return(n_valid_sessions)

}

#' Create a figure showing technology operational period and how many files per night
#'
#' @param dataframe Results dataframe
#'
#' @return A ggplot barplot of recording session file counts e.g., number of images
#' @export
#'
plot_device_operation_barplot <- function(dataframe){

  # Count number of files per recording session per deployment
  session_counts <- dataframe %>%
    dplyr::group_by(site_name, recording_session) %>%
    dplyr::summarise(n_files = n_distinct(filepath), .groups = "drop") %>%
    dplyr::filter(n_files >= 10)

  number_operational_sessions <- calculate_device_operation(dataframe)

  # Plot
  barplot <- ggplot2::ggplot(session_counts, aes(x = recording_session, y = n_files)) +
    ggplot2::geom_col(fill = "steelblue") +
    labs(
      title = sprintf("Total recording sessions = %s", number_operational_sessions),
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
#' @return A ggplot tileplot of recording session operation
#' @export
#'
plot_device_operation_tileplot <- function(dataframe){

  operational_sessions_df <- dataframe %>%
    dplyr::distinct(site_name, recording_session)

  tileplot <- ggplot2::ggplot(operational_sessions_df, aes(x = recording_session, y = site_name)) +
    ggplot2::geom_tile(width = 1, height = 0.9, fill = "steelblue") +
    scale_x_date(
      limits = c(
        floor_date(min(ymd(operational_sessions_df$recording_session)), "month"),
        ceiling_date(max(ymd(operational_sessions_df$recording_session)), "month")
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

#' Create a 3rd option for a figure showing technology operational period using calendar plot
#'
#' @param dataframe Results dataframe
#' @import openair
#' @importFrom tidyr complete
#'
#' @return An openair calendar plot of device operation
#' @export
#'
plot_device_operation_calendar <- function(dataframe){

  # Count number of files per night per deployment
  session_counts <- dataframe %>%
    dplyr::group_by(site_name, recording_session) %>%
    dplyr::summarise(n_files = n_distinct(filepath), .groups = "drop") %>%
    dplyr::filter(n_files >= 10)

  calendar_data <- session_counts %>%
    mutate(date = recording_session) %>%
    complete(
      site_name,
      date = seq(min(date), max(date), by = "day")
    )

  year_to_plot <- unique(year(calendar_data$date))

  if(length(year_to_plot) != 1){
    stop(
      paste(
        "Data must contain exactly one year.",
        "Years detected:", paste(year_to_plot, collapse = ", ")
      )
    )
  }

  month_range <- range(lubridate::month(calendar_data$date), na.rm = TRUE)
  months_to_plot <- seq(month_range[1], month_range[2])

  calendar_plot <- openair::calendarPlot(
    calendar_data,
    date = "date",
    pollutant = "n_files",
    year = year_to_plot,
    month = months_to_plot,
    key.header = "Number of files",
    key.position	= "top"
  )

  return(calendar_plot)

}
