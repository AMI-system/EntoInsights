#
#' Validate the existence of a credentials file and warn about security risks
#'
#' This function checks whether the specified credentials file exists. If it does not,
#' an error is raised. If it does, a warning is issued about the potential risks of sharing 
#' credentials or using them outside of the EntoInsights package.
#'
#' @param credentials_path Character. The path to the credentials file.
#'
#' @return NULL (invisibly). The function stops execution if the credentials file does not exist.
#'
credentials_check <- function(credentials_path) {
  if (!file.exists(credentials_path)) {
    stop("You have not specified the correct path to your credentials file.")
  } else {
    warning("Your credentials file gives you the freedom to download as well as delete files in the object store. ",
            "Please do not share your credentials with others, and do not use it for functions outside of the EntoInsights package ",
            "unless you are certain it will not delete any files.")
  }

  return(invisible(NULL))
}