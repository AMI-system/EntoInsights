#' @import dplyr
#' @importFrom magrittr %>%
NULL # <- this line tells roxygen the imports are not attached to any function

#' Download files from an S3 bucket
#'
#' This function downloads files from an S3 bucket based on provided vectors of deployment IDs,
#' data types, and filenames. The files are downloaded in parallel with the function outputting a log with the final download status of each file.
#'
#' @param bucket Character. Name of the S3 bucket e.g., gbr
#' @param deployment_id Character vector or single value. Deployment ID corresponding to each file.
#'  If you are uncertain of this, see the image_path of your classifications, as the ID should be in the path.
#' @param data_type Character vector or single value. Data type(s) corresponding to each file.
#'  E.g., snapshot_images, motion_images, ultrasound_recordings, or audible_recordings
#' @param filename Character vector. List of basename filenames to download.
#' @param download_path Character. Local directory where files will be downloaded. Default is "./downloads".
#' @param credentials_path Character. Path to credentials file JSON.
#' @param save_download_log Logical. Whether to save the download log.
#' @import tibble
#' @import fs
#' @import furrr
#' @import future
#' @import purrr
#' @import paws
#' @importFrom jsonlite fromJSON
#' @return A data frame containing deployment IDs, data types, local file paths, and download status. The download status is either, 'successfully downloaded' meaning the file has been downloaded to the download directory, 'already downloaded (skipped)' meaning the file already exists in the download directory, or 'failed download (see warnings)'. This means that there was an error that interrupted the download process. Please check the last warnings with warnings() for more details.
#' @export
download_object_store_files <- function(bucket, deployment_id, data_type, filename,
                                        download_path = "./downloads", credentials_path = "./credentials.json",
                                        save_download_log = TRUE) {

  # Warning message
  credentials_check(credentials_path)

  # Validate lengths
  n <- length(filename)
  if (length(bucket) != 1 && length(bucket) != n)
    stop("bucket must be either a single value or the same length as filename.")
  if (length(deployment_id) != 1 && length(deployment_id) != n)
    stop("deployment_id must be either a single value or the same length as filename.")
  if (length(data_type) != 1 && length(data_type) != n)
    stop("data_type must be either a single value or the same length as filename.")

  # Recycle vectors if needed
  if (length(bucket) == 1) bucket <- rep(bucket, n)
  if (length(deployment_id) == 1) deployment_id <- rep(deployment_id, n)
  if (length(data_type) == 1) data_type <- rep(data_type, n)

  # Create full table
  file_details_df <- tibble::tibble(
    bucket = bucket,
    deployment_id = deployment_id,
    data_type = data_type,
    filename = filename
  ) %>% dplyr::distinct()

  # Load credentials once
  aws_credentials <- jsonlite::fromJSON(normalizePath(credentials_path))

  # Ensure download folder exists
  if (!fs::dir_exists(download_path)) {
    fs::dir_create(download_path)
    message("Created download path: ", download_path)
  }

  # Process each bucket one at a time
  all_results <- list()
  plan(multisession)  # Use parallel processing within each bucket

  # Create S3 client inside worker
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

  for (b in unique(file_details_df$bucket)) {
    message("Processing bucket: ", b)
    bucket_subset <- dplyr::filter(file_details_df, bucket == b)

    results <- furrr::future_map(seq_len(nrow(bucket_subset)), function(i) {
      row <- bucket_subset[i, ]
      key <- file.path(row$deployment_id, row$data_type, row$filename)
      local_path <- file.path(download_path, key)
      fs::dir_create(fs::path_dir(local_path))

      if (fs::file_exists(local_path)) {
        message("File already downloaded, skipping: ", key)
        return(data.frame(bucket = row$bucket,
                          deployment_id = row$deployment_id,
                          data_type = row$data_type,
                          local_path = local_path,
                          download_status = "already downloaded (skipped)",
                          stringsAsFactors = FALSE))
      }

      tryCatch({
        object <- s3$get_object(Bucket = row$bucket, Key = key)
        writeBin(object$Body, local_path)
        return(data.frame(bucket = row$bucket,
                          deployment_id = row$deployment_id,
                          data_type = row$data_type,
                          local_path = local_path,
                          download_status = "successfully downloaded",
                          stringsAsFactors = FALSE))
      }, error = function(e) {
        warning(paste("Error downloading", key, ":", paste(e$message, collapse = " ")))
        return(data.frame(bucket = row$bucket,
                          deployment_id = row$deployment_id,
                          data_type = row$data_type,
                          local_path = local_path,
                          download_status = "failed download (see warnings)",
                          stringsAsFactors = FALSE))
      })
    }, .options = furrr::furrr_options(seed = TRUE))

    all_results[[b]] <- do.call(rbind, results[!sapply(results, is.null)])
  }

  downloaded_files <- dplyr::bind_rows(all_results)

  if (save_download_log) {
    utils::write.csv(downloaded_files, "files_downloaded_log.csv", row.names = FALSE)
    message("Download log saved as 'files_downloaded_log.csv'")
  }

  return(downloaded_files)
}

#' Download top N images with most detections
#'
#' Identifies the images with the highest number of unique detections
#' (based on `crop_status`) and downloads the original image files
#' from the object store.
#'
#' @param dataframe Data frame containing inference results.
#' @param n Integer. Number of top images to download. Default = 3.
#' @param data_type Character. Data type folder in bucket
#'  e.g., snapshot_images, motion_images, ultrasound_recordings, or audible_recordings.
#' @param download_path Character. Local download directory.
#' @param credentials_path Character. Path to AWS credentials JSON.
#' @param save_download_log Logical. Whether to save download log.
#'
#' @return Data frame of download results.
#'
#' @details
#' This function  calls \code{\link{download_object_store_files}} internally.
#' @seealso download_object_store_files
#' @export
download_top_detection_images <- function(dataframe,
                                          n = 3,
                                          data_type,
                                          download_path = "./downloads",
                                          credentials_path = "./credentials.json",
                                          save_download_log = TRUE) {

  # Validate required columns
  required_cols <- c("bucket_name", "deployment_id", "recording_session",
                     "image_path_basename", "crop_status")
  missing_cols <- setdiff(required_cols, names(dataframe))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Count unique detections per image
  detection_counts <- dataframe %>%
    dplyr::filter(!is.na(crop_status),
                  crop_status != "",
                  crop_status != "No detections for this image.") %>%
    dplyr::group_by(bucket_name, deployment_id, recording_session, image_path_basename) %>%
    dplyr::summarise(
      n_detections = dplyr::n_distinct(crop_status),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(n_detections)) %>%
    dplyr::group_by(recording_session) %>%
    dplyr::slice_max(n_detections, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(n_detections))

  if (nrow(detection_counts) == 0) {
    stop("No images with detections found.")
  }

  if (n > nrow(detection_counts)) {
    warning("Requested n is greater than available sessions. Returning all available images.")
  }

  top_images <- detection_counts %>%
    dplyr::slice_head(n = n)

  # Call your existing function
  df <- download_object_store_files(
    bucket = top_images$bucket_name,
    deployment_id = top_images$deployment_id,
    data_type = data_type,
    filename = top_images$image_path_basename,
    download_path = download_path,
    credentials_path = credentials_path,
    save_download_log = save_download_log
  )

  return(df$local_path)

}
