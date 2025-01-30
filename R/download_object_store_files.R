#' Download files from an S3 bucket
#'
#' This function downloads files from an AWS S3 bucket based on deployment ID,
#' filename, and classification ID. It ensures the directory structure exists,
#' supports parallel downloading, and returns a dataframe with the local file paths.
#'
#' @param bucket A character string specifying the name of the S3 bucket.
#' @param deployment_id A character vector of deployment IDs.
#' @param data_type A character string specifying the type of data (used to construct the S3 key).
#' @param filename A character vector of filenames corresponding to the deployment IDs.
#' @param classification_id A character vector of classification IDs, must have the same length as `deployment_id` and `filename`.
#' @param download_path A character string specifying the local directory where files should be downloaded. Default is "./downloads".
#'
#' @return A dataframe containing `deployment_id`, `local_path`, and `classification_id` for the downloaded files.
#'
#' @details
#' - The function reads AWS credentials from a JSON file (`./credentials.json`).
#' - It uses the `paws` package to interact with AWS S3.
#' - Parallel downloading is enabled using the `furrr` package.
#' - If any downloads fail, an error message is displayed, and the function proceeds with the remaining downloads.
#'
#'
#' @import jsonlite
#' @import tibble
#' @import fs
#' @import furrr
#' @export

download_object_store_files <- function(bucket, deployment_id, data_type, filename, classification_id = NULL, download_path = "./downloads") {
  # Validate inputs

  if (is.null(classification_id)){
    print("generating classification IDs")
    classification_id = 1:length(filename)
  }

  if (length(unique(lengths(list(deployment_id, filename, classification_id)))) > 1) {
    stop("deployment_id, filename, and classification_id must have the same length.")
  }
  
  # Create a dataframe from inputs
  file_details_df <- tibble(
    deployment_id = deployment_id,
    filename = filename,
    classification_id = classification_id
  )
  
  # Initialize S3 client
  aws_credentials <- fromJSON("./credentials.json")
  s3 <- paws::s3(
    config = list(
      credentials = list(
        creds = list(
          access_key_id = aws_credentials$AWS_ACCESS_KEY_ID,
          secret_access_key = aws_credentials$AWS_SECRET_ACCESS_KEY
        )
      ),
      region = aws_credentials$AWS_REGION,
      endpoint = aws_credentials$AWS_URL_ENDPOINT
    )
  )
  
  # Ensure the download path exists
  if (!dir_exists(download_path)) {
    dir_create(download_path)
    message("Created download path: ", download_path)
  }
  
  # Download objects
  plan(multisession)  # Use parallel processing for downloads
  results <- future_map(
    seq_len(nrow(file_details_df)),
    function(i) {
      row <- file_details_df[i, ]
      
      bucket_name <- bucket
      deployment_id <- row$deployment_id
      filename <- row$filename
      classification_id = row$classification_id
      
      # Construct the S3 key and local download path
      key <- file.path(deployment_id, data_type, filename)
      local_path <- file.path(download_path, key)
      
      # Ensure the local directory structure exists
      dir_create(path_dir(local_path))
      
      # Download the object
      tryCatch({
        object <- s3$get_object(
          Bucket = bucket_name,
          Key = key
        )
        writeBin(object$Body, local_path)
        message("Downloaded: ", key)
        # Return a dataframe with the download result
        return(data.frame(deployment_id = deployment_id, local_path = local_path, classification_id = classification_id, stringsAsFactors = FALSE))
      }, error = function(e) {
        message("Error downloading ", key, ": ", e$message)
        # Return NULL on error
        return(NULL)
      })
    },
    .options = furrr_options(seed = TRUE)  # Ensure reproducibility
  )
  
  message("Download complete!")
  
  # Combine results into a single dataframe, excluding NULLs
  downloaded_files <- do.call(rbind, results[!sapply(results, is.null)])
  
  # Return the combined dataframe
  return(downloaded_files)
}
