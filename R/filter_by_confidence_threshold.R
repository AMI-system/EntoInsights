#' Filter the data according to a specified minimum species classifier confidence threshold.
#'
#' Only rows with species predictions will be included. Images with no detections or rows with detections of images other than moths will
#' be filtered out at this stage.
#'
#' @param dataframe Results dataframe
#' @param conf_threshold Value between 0 and 1 to set as the confidence threshold to filter the results. Defaults to 0.7.
#'
#' @return A filtered dataframe with only predictions over a certain confidence
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
filter_by_conf_threshold <- function(dataframe, conf_threshold = 0.7){

  filtered_dataframe <- dataframe %>%
    filter(species_confidence > conf_threshold)

  return(filtered_dataframe)

}
