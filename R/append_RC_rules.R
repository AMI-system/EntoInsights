#' Append Recording Confidence Rules
#'
#' This function appends columns onto a classifications dataframe indicating whether the classification is sensible regarding
#' spatial presence (1 or 0), phenology (1 or 0), and appends a column with a scale regarding identification difficulty (from 1 - 4, distinct to requiring dissection, or 5, meaning Ungraded).
#' The function uses preloaded 'rules' datasets provided by the NBN Record Cleaner. The function iterates through all detected top-n classifications
#' (e.g., `top_1_species`, `top_2_species`, etc.) and appends spatial presence, phenology, and identification difficulty for each classification separately.
#' Note, currently this application of the record cleaner does not assess results collected in Northern Ireland.
#'
#' @param classifications_df A dataframe containing classifications. This dataset assumes the data is in the format obtained from the classification pipeline on JASMIN.
#' @param latitude Latitude of the deployment.
#' @param longitude Longitude of the deployment.
#'
#' @return The updated classifications dataframe with appended RC rules.
#'
#' @details The data appended by this function is obtained from the NBN Record Cleaner.
#' If you use this function or the datasets in publications, please cite the NBN Record Cleaner.
#'
#' @references National Biodiversity Network (NBN) Record Cleaner.
#' See \url{https://nbn.org.uk/tools-and-resources/nbn-record-cleaner/}.
#'
#' @import dplyr
#' @import sf
#' @import stringr
#' @import lubridate
#' @export
append_RC_rules <- function(classifications_df, latitude, longitude) {

    # Access datasets directly from the package
    data("difficulty_description", package = "EntoInsights")
    data("id_difficulty", package = "EntoInsights")
    data("periodwithinyear", package = "EntoInsights")
    data("tenkm", package = "EntoInsights")

    # Simplify taxon name for joining
    id_difficulty <- id_difficulty %>% mutate(join_species_name = gsub("-", " ", tolower(taxon)))
    periodwithinyear <- periodwithinyear %>% mutate(join_species_name = gsub("-", " ", tolower(taxon)))
    tenkm <- tenkm %>% mutate(join_species_name = gsub("-", " ", tolower(taxon)))

    convert_to_km100_10 <- function(longitude, latitude) {
    # Create a single point in WGS84 (longitude, latitude)
    point <- st_sfc(st_point(c(longitude, latitude)), crs = 4326)

    # Transform to British National Grid (OSGB36, EPSG:27700)
    point_bng <- st_transform(point, 27700)

    # Extract easting and northing
    coords <- st_coordinates(point_bng)
    easting <- coords[1]
    northing <- coords[2]

    ### OSGB uses A-Z in a 5x5 grid except "I" (9th LETTER)
    ### Arrange the matrix correctly
    m = matrix(LETTERS[-9], ncol = 5, byrow = TRUE)[5:1, ]

    # 500k square letter
    xo = trunc(easting / 500000) + 3
    yo = trunc(northing / 500000) + 2
    # Lookup in the matrix (Y first)
    s1 = m[yo, xo]

    # 100k square letter
    xo = trunc((easting %% 500000) / 100000) + 1
    yo = trunc((northing %% 500000) / 100000) + 1
    # Lookup
    s2 = m[yo, xo]

    # Combine to form the km100 value
    km100 = paste0(s1, s2)

    # Calculate km10 value
    # Take remaining easting and northing after 100k, divide by 10k, and truncate
    km10_easting = trunc((easting %% 100000) / 10000)
    km10_northing = trunc((northing %% 100000) / 10000)
    km10 = paste0(km10_easting, km10_northing)

    # Return both km100 and km10
    return(list(km100 = km100, km10 = km10))
    }

    grid_ref <- convert_to_km100_10(longitude, latitude)

    if (is.na(grid_ref$km100) | is.na(grid_ref$km10)) {
    stop("Error: Grid reference calculation failed. Please check your coordinates.")
    }

    # Filter tenkm based on grid reference
    tenkm_site <- tenkm %>%
        filter(km100 == grid_ref$km100) %>%
        mutate(km10_list = str_split(km10, " ")) %>%
        filter(grid_ref$km10 %in% unlist(km10_list))

    # Process classifications_df
    classifications_df <- classifications_df %>%
      mutate(
        observation_date = date,
        observation_month = month(date),
        observation_day = day(date)
      )
        # mutate(
        #     observation_date = as.Date(parse_AMI_datetimes(image_path), format = "%Y%m%d"),
        #     observation_month = month(observation_date),
        #     observation_day = day(observation_date)
        # )

    # Loop through columns that match 'top_X_species'
    for (col in colnames(classifications_df)) {
        if (grepl("^top_\\d+_species$", col) | grepl("top_species_prediction", col)) {

            # Create a species lookup for the current column
            species_lookup_df <- classifications_df %>%
                select(observation_date, observation_month, observation_day, !!sym(col)) %>%
                distinct() %>%
                mutate(join_species_name = gsub("-", " ", tolower(!!sym(col)))) %>%
                left_join(id_difficulty, by = "join_species_name") %>%
                mutate(presence = ifelse(join_species_name %in% tenkm_site$join_species_name, 1, ifelse(join_species_name %in% unique(tenkm$join_species_name), 0, NA))) %>%
                left_join(periodwithinyear, by = "join_species_name") %>%
                mutate(
                    within_date = ifelse(
                        (observation_month > start_month |
                        (observation_month == start_month & observation_day >= start_day)) &
                        (observation_month < end_month |
                        (observation_month == end_month & observation_day <= end_day)),
                        1, 0
                    )
                ) %>%
                select(
                    observation_date,
                    join_species_name,
                    code,
                    presence,
                    within_date
                ) %>%
                rename(
                    !!paste0(col, "_identification_difficulty") := code,
                    !!paste0(col, "_presence") := presence,
                    !!paste0(col, "_within_date") := within_date
                )

            # Merge the lookup back into classifications_df
            classifications_df <- classifications_df %>%
                mutate(join_species_name = gsub("-", " ", tolower(!!sym(col)))) %>%
                left_join(species_lookup_df)
        }
    }

    # Remove the date columns I used previously
    classifications_df = classifications_df %>% select(-c(join_species_name, observation_date, observation_month, observation_day))

    return(classifications_df)
}

#' Remove implausible rows based on recording Confidence Rules
#'
#' This function removes rows in the dataframe where the prediction is implausible
#' based on location, date, and/or identification difficulty.
#'
#' @param dataframe_with_rc A dataframe containing classifications with record
#'   confidence rules appended. This dataset assumes the data is in the format
#'   obtained from the classification pipeline on JASMIN.
#' @param filter_location Logical. If TRUE (default), remove rows where
#'   `top_species_prediction_presence == 0`. NA values are always retained.
#' @param filter_date Logical. If TRUE (default), remove rows where
#'   `top_species_prediction_within_date == 0`. NA values are always retained.
#' @param filter_id_difficulty Logical. If TRUE (default), filter based on
#'   identification difficulty. NA values are always retained.
#' @param max_id_difficulty Integer. Maximum identification difficulty level
#'   to retain (default = 2, i.e., keep difficulty 1 or 2. Species with an identification
#'   difficulty of great than 2 are difficult to identify, and even experienced recorders
#'   may be expected to provide additional evidence). NA values are always retained.
#'
#' @return The updated dataframe with implausible predictions removed
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
remove_implausible_predictions <- function(dataframe_with_rc,
                                           filter_location = TRUE,
                                           filter_date = TRUE,
                                           filter_id_difficulty = TRUE,
                                           max_id_difficulty = 2){

  refined_dataframe <- dataframe_with_rc

  if (filter_location) {
    refined_dataframe <- refined_dataframe %>%
      filter(
        is.na(top_species_prediction_presence) |
          top_species_prediction_presence != 0
      )
  }

  if (filter_date) {
    refined_dataframe <- refined_dataframe %>%
      filter(
        is.na(top_species_prediction_within_date) |
          top_species_prediction_within_date != 0
      )
  }

  if (filter_id_difficulty) {
    refined_dataframe <- refined_dataframe %>%
      filter(
        is.na(top_species_prediction_identification_difficulty) |
          top_species_prediction_identification_difficulty <= max_id_difficulty
      )
  }

  return(refined_dataframe)
}
