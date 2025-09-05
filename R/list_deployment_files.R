#' List files from S3 deployments
#'
#' Lists all file keys under one or more (bucket, deployment_id, data_type)
#' prefixes in an S3-compatible object store. If `data_type` is NULL, lists all
#' default data types. Handles large deployments with pagination.
#'
#' @param bucket Character vector. One bucket for all deployments, or multiple
#'   buckets of the same length as `deployment_id`.
#' @param deployment_id Character vector. Deployment IDs to query.
#' @param data_type Character vector or NULL. Data type subfolder(s). If NULL,
#'   queries all known types: motion_images, snapshot_images,
#'   audible_recordings, ultrasound_recordings, terrestrial_recordings,
#'   aquatic_recordings.
#' @param credentials_path Character. Path to credentials JSON file.
#' @param save_extract Logical. Save the final listing to `file_save_path`.
#' @param file_save_path Character. Path for the saved CSV. Default
#'   "deployment_files_extract.csv".
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item bucket_name
#'     \item deployment_id
#'     \item data_type
#'     \item file_name (full key path)
#'   }
#' @import tibble dplyr furrr future purrr paws tidyr
#' @importFrom jsonlite fromJSON
#' @export
list_deployment_files <- function(bucket,
                                  deployment_id,
                                  data_type = NULL,
                                  credentials_path = "./credentials.json",
                                  save_extract = TRUE,
                                  file_save_path = "deployment_files_extract.csv") {

  credentials_check(credentials_path)

  # ----- Validate buckets -----
  if (length(bucket) == 1 && length(deployment_id) != 1) {
    message("One bucket provided. Applying to all deployments.")
    bucket <- rep(bucket, length(deployment_id))
  } else if (length(bucket) != length(deployment_id)) {
    stop("If multiple buckets are provided, they must be equal in length to deployment_id.")
  }

  # ----- Default data types -----
  if (is.null(data_type)) {
    data_type <- c(
      "motion_images",
      "snapshot_images",
      "audible_recordings",
      "ultrasound_recordings",
      "terrestrial_recordings",
      "aquatic_recordings"
    )
  }

  # ----- Expand to full combinations -----
  combos <- tibble::tibble(bucket = bucket, deployment_id = deployment_id) %>%
    tidyr::crossing(data_type = data_type) %>%
    distinct()

  # ----- Load credentials -----
  aws_credentials <- jsonlite::fromJSON(normalizePath(credentials_path))

  # Preserve original plan, set parallel execution
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(multisession)

  # Helper to list all keys for one prefix
  list_all_keys_for_prefix <- function(.bucket, .prefix, aws_credentials) {
    creds_list <- list(
      access_key_id = aws_credentials$AWS_ACCESS_KEY_ID,
      secret_access_key = aws_credentials$AWS_SECRET_ACCESS_KEY
    )
    if (!is.null(aws_credentials$AWS_SESSION_TOKEN)) {
      creds_list$session_token <- aws_credentials$AWS_SESSION_TOKEN
    }

    s3 <- paws::s3(config = list(
      credentials = list(creds = creds_list),
      region = aws_credentials$AWS_REGION,
      endpoint = aws_credentials$AWS_URL_ENDPOINT
    ))

    keys <- character(0)
    token <- NULL

    repeat {
      resp <- try(
        s3$list_objects_v2(
          Bucket = .bucket,
          Prefix = .prefix,
          ContinuationToken = token,
          MaxKeys = 1000
        ),
        silent = TRUE
      )

      if (inherits(resp, "try-error")) {
        warning(sprintf("Failed to list %s/%s", .bucket, .prefix))
        break
      }

      if (!is.null(resp$Contents) && length(resp$Contents) > 0) {
        page_keys <- vapply(resp$Contents, function(x) x$Key, character(1))
        keys <- c(keys, page_keys)
      }

      if (isTRUE(resp$IsTruncated)) {
        token <- resp$NextContinuationToken
      } else break
    }

    keys[keys != .prefix]
  }

  # Query all combinations in parallel
  results <- furrr::future_map(
    seq_len(nrow(combos)),
    function(i) {
      row <- combos[i, ]
      prefix <- file.path(row$deployment_id, row$data_type, fsep = "/")
      if (!grepl("/$", prefix)) prefix <- paste0(prefix, "/")

      keys <- list_all_keys_for_prefix(row$bucket, prefix, aws_credentials)

      if (length(keys) == 0) {
        return(tibble::tibble(
          bucket_name = character(0),
          deployment_id = character(0),
          data_type = character(0),
          file_name = character(0)
        ))
      }

      tibble::tibble(
        bucket_name = row$bucket,
        deployment_id = row$deployment_id,
        data_type = row$data_type,
        file_name = keys
      )
    },
    .options = furrr::furrr_options(seed = TRUE)
  )

  files <- dplyr::bind_rows(results)

  if (save_extract) {
    utils::write.csv(files, file_save_path, row.names = FALSE)
    message("File listing saved to: ", file_save_path)
  }

  return(files)
}
