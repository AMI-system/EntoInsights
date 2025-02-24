#' Download files from an S3 bucket
#'
#' This function downloads files from an S3 bucket given a list of deployment IDs,
#' data types, filenames, and classification IDs. Files are downloaded in parallel.
#'
#' @param bucket Character. Name of the S3 bucket.
#' @param deployment_id Character vector or single value. Deployment ID corresponding to each file.
#' @param data_type Character vector or single value. Data type(s) corresponding to each file.
#' @param filename Character vector. List of filenames to download.
#' @param classification_id Numeric or character vector. Vector of classification IDs. This will often be your row number.
#' @param download_path Character. Local directory where files will be downloaded. Default is "./downloads".
#' @param credentials_path Character. Path to credentials file JSON.
#' @param save_download_log Logical. Whether to save download log.
#' @import jsonlite
#' @import tibble
#' @import fs
#' @import furrr
#' @import future
#' @import purrr
#' @import paws.storage
#' @return A data frame containing deployment IDs, data types, local file paths, classification IDs, and download status.
#' @export
download_object_store_files <- function(bucket, deployment_id, data_type, filename, classification_id, 
                                        download_path = "./downloads", credentials_path = "./credentials.json", 
                                        save_download_log = TRUE) {
  
  # Warning message
  credentials_check(credentials_path)

  if (length(data_type) != 1 && length(data_type) != length(filename)) {
    stop("data_type must be either a single value or the same length as the filename vector.")
  }

  if (length(deployment_id) != 1 && length(deployment_id) != length(filename)) {
    stop("deployment_id must be either a single value or the same length as the filename vector.")
  }

  # Repeat data_type and deployment_id if it's a single value
  if (length(data_type) == 1) {
    data_type <- rep(data_type, length(filename))
  }
  if (length(deployment_id) == 1) {
    deployment_id <- rep(deployment_id, length(filename))
  }
  
  # Create a dataframe from inputs
  file_details_df <- tibble(
    deployment_id = deployment_id,
    data_type = data_type,
    filename = filename,
    classification_id = classification_id
  )
  
  # Initialize S3 client
  aws_credentials <- fromJSON(credentials_path)
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
      data_type <- row$data_type
      filename <- row$filename
      classification_id <- row$classification_id
      
      # Construct the S3 key and local download path
      key <- file.path(deployment_id, data_type, filename)
      local_path <- file.path(download_path, key)
      
      # Ensure the local directory structure exists
      dir_create(path_dir(local_path))

      # Check if file already exists before downloading
      if (file_exists(local_path)) {
        message("File already donwloaded, skipping: ", key)
        return(data.frame(deployment_id = deployment_id, 
                          data_type = data_type, 
                          local_path = local_path, 
                          classification_id = classification_id,
                          stringsAsFactors = FALSE))
      }

      # Attempt to download the object
      tryCatch({
        object <- s3$get_object(
          Bucket = bucket_name,
          Key = key
        )
        writeBin(object$Body, local_path)
        message("Downloaded: ", key)
        return(data.frame(deployment_id = deployment_id, 
                          data_type = data_type, 
                          local_path = local_path, 
                          classification_id = classification_id,
                          stringsAsFactors = FALSE))
      }, error = function(e) {
        Warning("Error downloading ", key, ": ", e$message)
        return(NULL)
      })
    },
    .options = furrr_options(seed = TRUE)  # Ensure reproducibility
  )
  
  message("Download process complete!")

  # Combine results into a single dataframe, ensuring NULL values are excluded
  downloaded_files <- do.call(rbind, results[!sapply(results, is.null)])

  # Save download log if enabled
  if (save_download_log) {
    write.csv(downloaded_files, "files_downloaded_log.csv", row.names = FALSE)
    message("Download log saved as 'files_downloaded_log.csv'")
  }
  
  # Return the combined dataframe
  return(downloaded_files)
}
