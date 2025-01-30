#' Generate AWS Credentials JSON File
#'
#' This function creates a JSON file containing AWS credentials and endpoint details.
#'
#' @param access_key_id A string containing the AWS Access Key ID.
#' @param secret_access_key A string containing the AWS Secret Access Key.
#' @param file_path A string specifying the file path where the JSON file should be saved. Default is "credentials.json".
#'
#' 
#' @return The function writes a JSON file to the specified location and returns the file path.
#' @import jsonlite
#' @export
#'
#' @examples
#' generate_aws_json("my_access_key", "my_secret_key")
#'
#' # Custom file path
#' generate_aws_json("my_access_key", "my_secret_key", "custom_path.json")

create_credentials <- function(access_key_id, secret_access_key, file_path = "credentials.json") {
  # Define AWS credentials structure
  aws_credentials <- list(
    AWS_ACCESS_KEY_ID = access_key_id,
    AWS_SECRET_ACCESS_KEY = secret_access_key,
    AWS_REGION = "eu-west-2",
    AWS_URL_ENDPOINT = "https://ami-test-o.s3-ext.jc.rl.ac.uk"
  )
  
  # Convert to JSON and write to file
  write_json(aws_credentials, file_path, pretty = TRUE, auto_unbox = TRUE)
  
  return(file_path)
}
