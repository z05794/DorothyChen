#' Leaflet map of earthquakes
#' Creates a map of selected earthquakes by country and time.
#' @param df A data frame containing cleaned NOAA earthquake data
#' @param annotation_col A character. Name of a descrptive vector in the data.
#' @return A leaflet map with earthquakes and annotations.
#' @export
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @examples
#' \dontrun{
#' eq_map(df, annotation_col = "location_name")
#' }

eq_map <- function(df, annotation_col) {

  map <-
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = data$longitude, lat = data$latitude,
                              radius = data$eq_primary, weight = 1,
                              popup = data[[annotation_col]])
  map
}


#' Creates a label for leaflet map
#' @param data A data frame containing cleaned NOAA earthquake data
#' @export
#' @examples
#' \dontrun{
#' eq_create_label(df)
#' }
#' @return A character vector with labels
#' 
eq_create_label <- function(df) {
  popup_text <- with(df, {
    location_name <- ifelse(is.na(location_name), "",
                            paste("<strong>Location:</strong>", location_name))
    eq_primary <- ifelse(is.na(eq_primary), "",
                         paste("<br><strong>Magnitude</strong>", eq_primary))
    deaths <- ifelse(is.na(deaths), "",
                     paste("<br><strong>Total deaths:</strong>", deaths))
    paste0(location_name, eq_primary, deaths)
  })
}
