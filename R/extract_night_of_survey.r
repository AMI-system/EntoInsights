#' Extract Night of Survey
#'
#' This function assigns a "night of survey" based on a given timestamp column,
#' considering a specified start and end time for the survey period.
#'
#' @param data A dataframe containing a column with timestamps.
#' @param timestamp_col A string specifying the name of the timestamp column.
#' @param start_time A string representing the survey start time (e.g., "18:00" or "18:30").
#' @param end_time A string representing the survey end time (e.g., "06:00" or "06:30").
#' @param time_zone A string representing the timezone of the start and end time (e.g., "UTC").
#'
#' @return The original dataframe with an added `night_of` column, indicating the night the data belongs to.
#' @export
#' 
extract_night_of_survey <- function(timestamp, start_time, end_time, timezone) {
  
  # Extract date and time
  date <- as.Date(timestamp)
  
  # Convert start_time and end_time to POSIXct for comparison
  start_time <- as.POSIXct(paste(date, start_time), format = "%Y-%m-%d %H:%M", tz=timezone)
  #end_time <- as.POSIXct(paste(date, end_time), format = "%Y-%m-%d %H:%M", tz=timezone)
  
  # If the time is before the start time, assign to the previous day's night
  night_of <- ifelse(timestamp >= start_time, date, date - 1)
  
  return(night_of)
}