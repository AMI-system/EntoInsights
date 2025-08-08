#' Download Hourly Environmental Data from NASA POWER API with Elevation + Moonlight
#'
#' @description
#' Downloads hourly environmental data from the NASA POWER API for one or more
#' locations, appends site elevation (via **elevatr**), and adds moonlight
#' fields from **moonlit** (`night`, `moonlightModel`, `moonPhase`).
#'
#' Time zones are handled as follows:
#' 1) The user-supplied `start_datetime` and `end_datetime` (with an explicit
#'    time zone) are converted to **UTC** to build the POWER API request window
#'    (YYYYMMDD).  
#' 2) After retrieval, timestamps are **back-converted** to the original local
#'    time zone and filtered to the exact interval.  
#' 3) Moonlight calculations are performed on the **local** datetimes, as
#'    expected by `moonlit::calculateMoonlightIntensity()`.
#'
#' @param latitudes Numeric vector of latitudes.
#' @param longitudes Numeric vector of longitudes (same length as `latitudes`).
#' @param start_datetime POSIXct (with time zone). Start of range.
#' @param end_datetime POSIXct (with time zone). End of range.
#' @param derive_e_from_elevation Logical. If `TRUE` (default), map elevation to
#'   extinction coefficient `e` using buckets: elevation < 500 m → 0.28; < 1000 m
#'   → 0.24; < 2000 m → 0.21; ≥ 2000 m → 0.16. If `FALSE`, use `e_default`.
#' @param e_default Numeric. Extinction coefficient used when `derive_e_from_elevation = FALSE`. Default `0.28`.
#'
#' @return A `data.frame` with one row per site-hour containing:
#' \itemize{
#'   \item `latitude`, `longitude`
#'   \item `datetime` (local time zone from the inputs)
#'   \item Environmental variables (renamed):
#'     \itemize{
#'       \item `temperature_2m` (from `T2M`, °C)
#'       \item `wind_speed_2m` (from `WS2M`, m/s)
#'       \item `wind_direction_2m` (from `WD2M`, degrees from North)
#'       \item `preparation_corrected` (from `PRECTOTCORR`, mm/hr)
#'       \item `relative_humidity_2m` (from `RH2M`, \%)
#'       \item `cloud_amount` (from `CLOUD_AMT`, \%)
#'       \item `root_soil_wetness` (from `GWETROOT`, 0–1)
#'       \item `surface_soil_wetness` (from `GWETTOP`, 0–1)
#'     }
#'   \item `elevation` (metres)
#'   \item `extinction_coefficient` (mapped `e`)
#'   \item `night` (logical), `moonlight_model` (relative to average full moon),
#'         `moon_phase` (\% illuminated)
#' }
#'
#'
#' @examples
#' \dontrun{
#' result <- get_hourly_env_data(
#'   latitudes = c(9.163544, 9.1619212),
#'   longitudes = c(-79.8378812, -79.8388263),
#'   start_datetime = as.POSIXct("2024-04-01 00:00:00", tz = "America/Panama"),
#'   end_datetime = as.POSIXct("2024-08-01 23:59:59", tz = "America/Panama")
#' )
#' }
#'
#' @seealso
#' \itemize{
#'   \item NASA POWER API (temporal/hourly/point): https://power.larc.nasa.gov/
#'   \item \code{moonlit::calculateMoonlightIntensity}
#'   \item \code{elevatr::get_elev_point}
#' }
#'
#' @importFrom httr GET timeout content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue_data
#' @importFrom dplyr bind_rows arrange left_join mutate select distinct filter rename
#' @importFrom sf st_as_sf st_crs st_coordinates
#' @importFrom elevatr get_elev_point
#' @importFrom lubridate with_tz
#' @import moonlit
#' @export
#' 
get_hourly_env_data <- function(latitudes, longitudes,
                                start_datetime, end_datetime,
                                derive_e_from_elevation = TRUE,
                                e_default = 0.28) {

  if(length(latitudes) != length(longitudes)){
    stop("Your latitude and longitude values are not equal in length")
  }

  if(is.null(attr(start_datetime, "tzone")) || is.null(attr(end_datetime, "tzone"))){
    stop("Your start and/or end timezone is not specified")
  }

  if(attr(start_datetime, "tzone") != attr(end_datetime, "tzone")){
    stop("Your start and/or end timezones are not the same")
  }

  local_tz = attr(start_datetime, "tzone")

  message(paste("Using timezone:", local_tz))

  if(local_tz != "UTC"){

    start_datetime <- lubridate::with_tz(start_datetime, "UTC")
    end_datetime <- lubridate::with_tz(end_datetime, "UTC")
    message(paste("As required by the NASA power API, the date has been converted to UTC. This will later be back-converted to", local_tz))
  }

  # NASA POWER wants YYYYMMDD
  start_date_string <- format(start_datetime, "%Y%m%d")
  end_date_string <- format(end_datetime,   "%Y%m%d")

  message(paste("extracting data between", format(start_datetime, "%Y-%m-%d"), "and", format(end_datetime, "%Y-%m-%d"), "(after conversion to UTC)"))

  parameters <- c("T2M","WS2M","WD2M","PRECTOTCORR","RH2M","CLOUD_AMT","GWETROOT","GWETTOP")
  parameter_string <- paste(parameters, collapse = ",")

  request_template <- paste0(
    "https://power.larc.nasa.gov/api/temporal/hourly/point?",
    "parameters={param_string}&community=RE&longitude={lon}&latitude={lat}&",
    "start={start}&end={end}&format=JSON"
  )

  # Build per-site requests
  requests <- lapply(seq_along(latitudes), function(i) {
    lat <- latitudes[i]; lon <- longitudes[i]
    url <- glue::glue_data(
      list(param_string = parameter_string, lon = lon, lat = lat,
           start = start_date_string, end = end_date_string),
      request_template
    )
    list(request = url, lat = lat, lon = lon)
  })

  # Worker: fetch POWER data for a single site
  download_from_power_API <- function(req) {

    lat <- req$lat
    lon <- req$lon
    
    out <- try({
      resp <- httr::GET(req$request, httr::timeout(5*60)) # 5 minutes
      txt  <- httr::content(resp, as = "text", encoding = "UTF-8")
      json <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
      param_data <- json$properties$parameter

      all_hours <- unique(unlist(lapply(param_data, names)))

      # POWER hours are YYYYMMDDHH in UTC
      df <- data.frame(datetime = as.POSIXct(all_hours, format = "%Y%m%d%H", tz = "UTC"))

      # Add each parameter column
      for (p in names(param_data)) {
        df[[p]] <- as.numeric(unlist(param_data[[p]]))
      }

      df$latitude  <- lat
      df$longitude <- lon

      df

    }, silent = TRUE)

    if (inherits(out, "try-error")) {
      print(out)
      message(paste("POWER request failed at:", lat, lon))
      return(NULL)
    }

    return(out)
  }

  # Download all sites (sequential for reliability; parallelise later if needed)
  site_power_API_dfs <- lapply(requests, download_from_power_API)

  # Order by latitude, longitude, and convert to local timezone. Filter the data from previous dates
  env_data <- dplyr::bind_rows(site_power_API_dfs) %>%
    dplyr::arrange(latitude, longitude, datetime) %>%
    mutate(datetime = lubridate::with_tz(datetime, local_tz)) %>%
    dplyr::filter(datetime >= start_datetime, datetime <= end_datetime)

  # Elevation per unique site
  site_coords <- data.frame(latitude = latitudes, longitude = longitudes)

  sf_pts <- sf::st_as_sf(site_coords, coords = c("longitude","latitude"), crs = 4326)
  elev_sf <- elevatr::get_elev_point(locations = sf_pts, prj = sf::st_crs(sf_pts), src = "aws")
  elev_df <- as.data.frame(elev_sf)
  elev_df$longitude <- sf::st_coordinates(elev_sf)[,1]
  elev_df$latitude  <- sf::st_coordinates(elev_sf)[,2]
  elev_df <- elev_df[, c("latitude","longitude","elevation")]

  env_data <- dplyr::left_join(env_data, elev_df, by = c("latitude","longitude"))

  #  Map elevation to extinction coefficient e (if requested) 
  elev_df = elev_df %>% filter(!is.na(elevation))

  if (derive_e_from_elevation) {
    site_e <- elev_df %>%
      dplyr::mutate(
        e = dplyr::case_when(
          elevation < 500 ~ 0.28,
          elevation < 1000 ~ 0.24,
          elevation < 2000 ~ 0.21,
          elevation >= 2000 ~ 0.16
        )
      ) %>%
      dplyr::select(latitude, longitude, e)
  } else {
    site_e <- elev_df %>%
      dplyr::mutate(e = e_default) %>%
      dplyr::select(latitude, longitude, e)
  }

  env_data <- dplyr::left_join(env_data, site_e, by = c("latitude","longitude"))

  # Compute moonlight for each site at hourly resolution
  add_moonlit_for_site <- function(df_site) {

    lat <- df_site$latitude[1]
    lon <- df_site$longitude[1]
    e   <- df_site$e[1]

    moonlit_extract <- moonlit::calculateMoonlightIntensity(
        lat = lat,
        lon = lon,
        date = df_site$datetime,
        e = e
      ) %>% select(night, moonlightModel, moonPhase)

    # Expect moonlit_extract to be aligned with the input vector
    df_site = cbind(df_site, moonlit_extract)

    df_site
  }

  env_data <- split(env_data, list(env_data$latitude, env_data$longitude), drop = TRUE) %>%
    lapply(add_moonlit_for_site) %>%
    dplyr::bind_rows() %>%
    rename(
      temperature_2m = T2M,
      wind_speed_2m = WS2M,
      wind_direction_2m = WD2M,
      precipitation_corrected = PRECTOTCORR,
      relative_humidity_2m = RH2M,
      cloud_amount = CLOUD_AMT,
      root_soil_wetness = GWETROOT,
      surface_soil_wetness = GWETTOP,
      extinction_coefficient = e,
      moonlight_model = moonlightModel,
      moon_phase = moonPhase
    ) %>%
    dplyr::arrange(latitude, longitude, datetime)

  return(env_data)
}
