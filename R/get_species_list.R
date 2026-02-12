#' Generate a unique species list for species detected over the deployment.
#'
#' Only include species predicted with a confidence over a specified threshold.
#'
#' @param dataframe Results dataframe
#' @param conf_threshold Value between 0 and 1 to set as the confidence threshold to filter the results. Defaults to 0.7.
#'
#' @return A species list
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
get_species_list <- function(dataframe, conf_threshold = 0.7){

  species_list <- dataframe %>%
    filter(species_confidence > conf_threshold) %>%
    distinct(top_species_prediction)

  return(species_list)

}
