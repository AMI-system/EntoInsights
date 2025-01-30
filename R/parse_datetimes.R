#' Extract and parse datetime from filenames
#'
#' This function extracts datetime strings from filenames, removes any underscores,
#' and converts them into datetime objects.
#'
#' @param file_names A character vector of filenames containing datetime patterns.
#'
#' @return A vector of parsed datetime objects in UTC timezone.
#'
#' @details
#' - The function looks for datetime patterns in the format `YYYYMMDD_HHMMSS` or `YYYYMMDDHHMMSS`.
#' - It removes any underscores found in the extracted datetime strings.
#' - Uses `lubridate::parse_date_time()` to convert strings into datetime objects.
#'
#' @examples
#' 
#' # Example filenames
#' file_names <- c("image_20240101_123456.jpg", "sensor20231231123059.csv")
#' 
#' # Parse datetimes
#' parsed_dates <- parse_datetimes(file_names)
#'
#' @import stringr
#' @import lubridate
#' @export

parse_AMI_datetimes <- function(file_names) {
  # Extract datetime patterns
  datetime_str <- str_extract(file_names, "\\d{8}_\\d{6}|\\d{14}")
  
  # Remove underscores if present
  datetime_str <- str_replace_all(datetime_str, "_", "")
  
  # Convert to datetime
  parse_date_time(datetime_str, orders = c("YmdHMS"), tz = "UTC")
}
