#' Create a Leaflet map showing technology locations (adaptable so can input more than one set of lat/lons for multiple technologies)
#'
#' @param latitude Numeric vector. Latitude(s) of the technology/ies.
#' @param longitude Numeric vector. Longitude(s) of the technology/ies.
#' @param labels Optional character vector. Labels to show in popups for each marker. Defaults to "AMI system".
#'
#' @return A leaflet map object
#' @export
#'
map <- function(latitude, longitude, labels = NULL) {

  # Check inputs
  if (!is.numeric(latitude) | !is.numeric(longitude)) {
    stop("Latitude and longitude must be numeric")
  }
  if (length(latitude) != length(longitude)) {
    stop("Latitude and longitude must have the same length")
  }

  if (is.null(labels)) {
    labels <- rep("AMI system", length(latitude))
  } else {
    if (length(labels) != length(latitude)) {
      stop("Length of labels must match number of coordinates")
    }
  }

  # Create the map
  map <- leaflet::leaflet() |>
    leaflet::addTiles() |>
    # Add circular markers
    leaflet::addCircleMarkers(
      lng = longitude,
      lat = latitude,
      radius = 15,                # make marker bigger
      color = "red",             # stand-out color
      fillOpacity = 0.9
    ) |>
    # Add always-visible labels
    leaflet::addLabelOnlyMarkers(
      lng = longitude,
      lat = latitude,
      label = labels,
      labelOptions = leaflet::labelOptions(
        noHide = TRUE,           # label always visible
        direction = "top",
        textOnly = TRUE,
        style = list(
          "color" = "black",
          "font-weight" = "bold",
          "font-size" = "18px",
          "background" = "rgba(255,255,255,0.7)",
          "padding" = "2px 4px",
          "border-radius" = "4px"
        )
      )
    )

  # Zoom to fit all markers if multiple
  if (length(latitude) > 1) {
    map <- map |> leaflet::fitBounds(
      lng1 = min(longitude, na.rm = TRUE),
      lat1 = min(latitude, na.rm = TRUE),
      lng2 = max(longitude, na.rm = TRUE),
      lat2 = max(latitude, na.rm = TRUE)
    )
  }

  return(map)
}
