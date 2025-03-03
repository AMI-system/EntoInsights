#' Crops snapshot images
#'
#' This function can be used to crop snapshots from the object store service. This function is useful for users intending to download subsets of data from a deployment or from multiple deployments.
#' To download greater numbers of files from the object store (e.g., all files in a deployment), please see the object store functions at \url{https://github.com/AMI-system/object-store-scripts/tree/image_download}..
#' Please note, to run this function you need to generate a credentials JSON file, which requires a key, managed by the object store data manager.
#' Once you have a key, you can generate a credentials object using the function, create_credentials().
#'
#' @param bucket A string with the bucket name, which is the iso3 code, in lowercase (e.g., "gbr").
#' @param deployment_id A vector of strings with a length equal to the number of files, and recording the deployment id of each image.
#' @param data_type A string with the type of data (one from: "snapshot_images", "audible_recordings", "ultrasound_recordings")
#' @param filename A vector of strings with a length equal to the number of files, and recording the file base name of each image.
#' @param classification_id This is an ID that you assign which allows you to safely map the species classifcation to the downloaded files given some classifications may share the same file.
#' 
#' @return NULL
#'
#' @import httr
#' @import paws
#' @import furrr
#' @import fs
#' @import dplyr
#' @import magick
#' @import stringr
#' @export
#' 

crop_and_save_images <- function(downloaded_files_dir = "./downloads", classifications_df, classifications_column = "top_1_species", output_dir = "./crops", save_classifications_crops_pairings = TRUE) {

  # Ensure the output directory exists
  if (!dir_exists(output_dir)) {
    dir_create(output_dir)
    message("Created output directory: ", output_dir)
  }

  required_columns <- c(classifications_column, "image_path")
  if (!all(required_columns %in% colnames(classifications_df))) {
    stop("The required columns are missing in classifications_df: ", paste(setdiff(required_columns, colnames(classifications_df)), collapse = ", "))
  }

  if (any(!c("deployment_id", "classification_id") %in% colnames(classifications_df))){
    warning("Deployment_id and/or classification id column is missing. Regenerating both in the output classifications dataframe")
  
    classifications_df <- prepare_classifications_df(classifications_df)
  }

  # List downloaded images in a dataframe alongside the deployment ID
  image_files <- list.files(downloaded_files_dir, recursive = TRUE)
  image_files_df <- data.frame(download_path = file.path(downloaded_files_dir, image_files), file_basename = basename(image_files), deployment_id = str_extract(image_files, "dep\\d{6}"))
  
  # Filter down the dataframe to include only images in the classification dataframe
  image_files_df <- image_files_df %>%
    semi_join(
      classifications_df %>%
        mutate(file_basename = basename(image_path)) %>%
        select(deployment_id, file_basename),
      by = c("deployment_id", "file_basename")
    )

  if(nrow(image_files_df) == 0){
    stop("There are no files in the downloads directory that present in the classifications dataframe.
    Have you specified the folder correct path for the download? Please make sure you have included the deployment ID folder in the path.")
  }

  # Iterate through the rows of downloaded files
  purrr::walk(seq_len(nrow(image_files_df)), function(i) {
    row <- image_files_df[i, ]
    
    deployment <- row$deployment_id
    basename <- row$file_basename
    download_path = row$download_path
    
    # Find the corresponding classification rows
    file_classification_rows <- classifications_df %>%
      filter(deployment_id == deployment, basename(image_path) == basename)

  # Read the image once to avoid reloading for each crop
  tryCatch({
    img <- image_read(download_path)

    for (classification_i in seq_len(nrow(file_classification_rows))) {
      x_min <- as.numeric(file_classification_rows$x_min[classification_i])
      y_min <- as.numeric(file_classification_rows$y_min[classification_i])
      x_max <- as.numeric(file_classification_rows$x_max[classification_i])
      y_max <- as.numeric(file_classification_rows$y_max[classification_i])
      class_name <- file_classification_rows[[classifications_column]][classification_i]
      classification_id <- file_classification_rows$classification_id[classification_i]

      # Check if species name is valid
      if (is.na(class_name) || is.null(class_name) || class_name == "") {
        stop("Some values in column, ", classifications_column,  ", are empty (NA, NULL, or empty strings)")
      }

      # Create class specific output directory
      class_dir <- file.path(output_dir, gsub(" ", "_", tolower(class_name)))
      if (!dir_exists(class_dir)) {
        dir_create(class_dir)
      }

      # Unique output file name per bounding box
      output_path <- file.path(class_dir, paste0(gsub(" ", "_", tolower(class_name)), "_",  classification_id, ".jpg"))

      # Crop the image using bounding box
      cropped_img <- image_crop(img, geometry = sprintf("%dx%d+%d+%d", x_max - x_min, y_max - y_min, x_min, y_min))
      image_write(cropped_img, path = output_path, format = "jpeg")
      message("Cropped and saved: ", output_path)
    }
  }, error = function(e) {
    message("Error processing file: ", download_file_path, ". Error: ", e$message)
  })
})

  # Save download log if enabled
  if (save_classifications_crops_pairings) {
    write.csv(classifications_df, "classifications_crops_pairings.csv", row.names = FALSE)
    message("classifications and crops pairings saved as 'classifications_crops_pairings.csv'")
  }

  return(NULL)

}