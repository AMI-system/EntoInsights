#' @title Download Hourly Environmental Data from NASA POWER API with Elevation + Moonlight
#'
#' @description
#' Downloads hourly environmental data from NASA POWER for one or more locations,
#' appends site elevation (elevatr), and adds moonlight fields (moonlit):
#'   - night, moonlightModel, moonPhase
#'
#' @param latitudes Numeric vector of latitudes.
#' @param longitudes Numeric vector of longitudes (same length as latitudes).
#' @param start_datetime POSIXct/Date. Start of range (inclusive).
#' @param end_datetime POSIXct/Date. End of range (inclusive).
#' @param tz Character. Time zone to present to `moonlit` (local time at site). Default "UTC".
#' @param derive_e_from_elevation Logical. If TRUE (default), map elevation to e:
#'        <=250m: 0.28; <=750m: 0.24; <=1500m: 0.21; >1500m: 0.16.
#'        If FALSE, use `e_default` for all sites.
#' @param e_default Numeric. Extinction coefficient if not deriving from elevation. Default 0.28.
#'
#' @return data.frame with latitude, longitude, datetime (UTC), elevation,
#'         POWER variables, and moonlight fields (night, moonlightModel, moonPhase).
#'
#' @importFrom httr GET timeout content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue_data
#' @importFrom dplyr bind_rows arrange left_join mutate select distinct
#' @importFrom sf st_as_sf st_crs st_coordinates
#' @importFrom elevatr get_elev_point
#' @importFrom lubridate with_tz
#' @import moonlit
get_hourly_env_data <- function(latitudes, longitudes,
                                start_datetime, end_datetime,
                                derive_e_from_elevation = TRUE,
                                e_default = 0.28) {

  if(length(latitudes) != length(longitudes)){
    stop("Your latitude and longitude values are not equal in length")
  }

  if(is.null(attr(start_datetime, "tzone")) || is.null(attr(end_datetime, "tzone"))){
    stop("your start and/or end timezone is not specified")
  }

  if(attr(start_datetime, "tzone") != attr(end_datetime, "tzone")){
    stop("your start and/or end timezones are not the same")
  }

  message(paste("using timezone", attr(start_datetime, "tzone")))

  start_date <- format(start_datetime, "%Y%m%d")
  end_date   <- format(end_datetime,   "%Y%m%d")

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
           start = start_date, end = end_date),
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
  site_dfs <- lapply(requests, download_from_power_API)

  browser()

  env_data <- dplyr::bind_rows(site_dfs) %>%
    dplyr::arrange(latitude, longitude, datetime)

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
          elevation < 500            ~ 0.28,
          elevation < 1000           ~ 0.24,
          elevation < 2000           ~ 0.21,
          elevation >= 2000          ~ 0.16
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
  add_moon_for_site <- function(df_site) {

    lat <- df_site$latitude[1]
    lon <- df_site$longitude[1]
    e   <- df_site$e[1]

    # Convert UTC be converted into the desired local tz
    # Requires lubridate::with_tz
    local_dt <- lubridate::with_tz(df_site$datetime, tzone = attr(start_datetime, "tzone"))

    moonlit_extract <- moonlit::calculateMoonlightIntensity(
        lat = lat,
        lon = lon,
        date = df_site$datetime,
        e = e
      ) %>% select(night, moonlightModel, moonPhase)

    # Expect moonlit_extract to be aligned with the input vector
    cbind(df_site, moonlit_extract)

    df_site
  }

  env_out <- do.call(rbind, lapply(split(env_data, list(env_data$latitude, env_data$longitude), drop = TRUE), add_moon_for_site))

  # Tidy column order
  env_out <- env_out %>%
    dplyr::arrange(latitude, longitude, datetime)

  return(env_out)
}
