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
#' @return A dataframe that allows you to track successful file downloads. The dataframe contains columns deployment_id, containing the deployment id, the local installation path, and the classification id.
#'
#' @import httr
#' @import paws
#' @import furrr
#' @import fs
#' @import dplyr
#' @import magick
#' @export

crop_and_save_images <- function(downloaded_files, classifications_df, output_dir = "./crops") {
  # Ensure the output directory exists
  if (!dir_exists(output_dir)) {
    dir_create(output_dir)
    message("Created output directory: ", output_dir)
  }
  
  # Check for required columns in classifications_df
  required_columns <- c("classification_id", "deployment_id")
  if (!all(required_columns %in% colnames(classifications_df))) {
    stop("The required columns are missing in classifications_df: ", paste(setdiff(required_columns, colnames(classifications_df)), collapse = ", "))
  }
  
  # Iterate through the rows of downloaded files
  purrr::walk(seq_len(nrow(downloaded_files)), function(i) {
    row <- downloaded_files[i, ]
    
    deployment <- row$deployment_id
    download_file_path <- row$local_path
    classification_id_number = row$classification_id
    
    # Find the corresponding classification row
    file_classification_row <- classifications_df %>%
      filter(deployment_id == deployment, basename(image_path) == basename(download_file_path), classification_id == classification_id_number)
    
    if (nrow(file_classification_row) == 0) {
      warning("No classification data found for file: ", download_file_path)
      return(NULL)
    }

    if(nrow(file_classification_row) > 1){
      stop("crop extraction unable to pair species with crop. Halting extraction")
    }
    
    # Extract bounding box and species name
    x_min <- as.numeric(file_classification_row$x_min)
    y_min <- as.numeric(file_classification_row$y_min)
    x_max <- as.numeric(file_classification_row$x_max)
    y_max <- as.numeric(file_classification_row$y_max)
    species_name <- file_classification_row$top_1_species

    if(is.na(species_name)||is.null(species_name)){
      message("Species name missing for file: ", download_file_path)
      return(NULL)
    } 
    if ((species_name == "")) {
      message("Species name missing for file: ", download_file_path)
      return(NULL)
    }
    
    # Generate output file name using classification_id
    species_dir = file.path(output_dir, gsub(" ", "_", species_name))

    if(!dir_exists(species_dir)){
      dir_create(species_dir)
    }

    output_path <- file.path(species_dir, paste0(gsub(" ", "_", species_name), "_", file_classification_row$classification_id, ".jpg"))
    
    # Crop the image using bounding box
    tryCatch({
      img <- image_read(download_file_path)
      cropped_img <- image_crop(img, geometry = sprintf("%dx%d+%d+%d", x_max - x_min, y_max - y_min, x_min, y_min))
      image_write(cropped_img, path = output_path, format = "jpeg")
      message("Cropped and saved: ", output_path)
    }, error = function(e) {
      message("Error processing file: ", download_file_path, ". Error: ", e$message)
    })
  })
}