#' Fetch deployment metadata from the AMI API
#'
#' Posts credentials to the AMI endpoint and returns the deployment table.
#'
#' @param api_username Character. API username.
#' @param api_password Character. API password.
#'
#' @return A dataframe with one row per deployment and deployment metadata.
#'
#' @examples
#' \dontrun{
#' deps <- get_deployment_table("API_username", "API_password")
#'
#' }
#'
#' @importFrom httr RETRY content http_error status_code timeout
#' @importFrom jsonlite fromJSON rbind_pages
#' @importFrom tibble as_tibble tibble
#' @import dplyr
#' 
#' @export
get_deployment_metadata <- function(api_username,
                                 api_password) {

  # POST credentials as form data with retries
  resp <- httr::RETRY(
    verb  = "POST",
    url   = "https://connect-apps.ceh.ac.uk/ami-data-upload/get-deployments/",
    body  = list(username = api_username, password = api_password),
    encode = "form",
    httr::timeout(600),
    times = 5,            # total attempts
    pause_min = 1,        # backoff starts at 1s
    pause_cap = 16,       # max backoff
    terminate_on = c(400, 401, 403) # don't keep retrying auth/perm errors
  )

  # Handle HTTP errors neatly
  if (httr::http_error(resp)) {
    sc <- httr::status_code(resp)
    txt <- tryCatch(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      error = function(e) ""
    )
    if (sc == 401) {
      stop("Unauthorised (401): wrong username or password.", call. = FALSE)
    }
    if (sc == 403) {
      stop("Forbidden (403): access denied for these credentials.", call. = FALSE)
    }
    stop(sprintf("HTTP error %s. Server said: %s", sc, txt), call. = FALSE)
  }

  # Parse JSON payload
  raw_txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (!nzchar(raw_txt)) {
    return(tibble::tibble())  # empty tibble if nothing returned
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(raw_txt, flatten = TRUE),
    error = function(e) {
      stop("Failed to parse JSON: ", e$message, call. = FALSE)
    }
  )

  # rename deployment_columns
  out = tibble::as_tibble(parsed) %>%
  rename(bucket = country_code, latitude = lat, longitude = lon)

  return(out)
}
