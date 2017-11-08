#' Plots a map showing locations of accidents
#'
#' Plot the exact location of the accident given the state number in a
#' particular year. This function stops if state.num is not a valid state id.
#' If there are no accidents, a message is printed as "no accidents to plot"
#' to the console.
#' @param state.num an interger representing a state.
#' @param year an interger representing a year
#' @return A map that gives the lattitude and lognitude of accidents in a
#' selected  year.
#' @examples
#' \donttest{
#' fars_map_state(1, 2014)
#' }
#' Uses the functions \code{make_filename} and \code{fars_read}.
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
