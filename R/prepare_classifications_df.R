#' Prepare Classifications Dataframe
#'
#' This function processes a classifications dataframe by extracting the deployment ID 
#' from the `image_path` column and adding a unique classification ID for each row.
#'
#' @param classifications_df A dataframe containing classifications. This dataframe 
#' is expected to have an `image_path` column from which the deployment ID will be extracted.
#'
#' @return A modified dataframe with two new columns:
#' \describe{
#'   \item{\code{deployment_id}}{Extracted deployment ID based on the pattern "dep\\d{6}".}
#'   \item{\code{classification_id}}{A unique row number assigned to each classification.}
#' }
#'
#' @import dplyr
#' @import stringr
#' @export
prepare_classifications_df <- function(classifications_df) {
  
    classifications_df <- classifications_df %>%
        mutate(deployment_id = str_extract(image_path, "dep\\d{6}"),
               classification_id = row_number())

    return(classifications_df)
}