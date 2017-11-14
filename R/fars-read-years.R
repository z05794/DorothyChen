#' Create a list of separate datasets by year
#'
#' Reads a data file from the current working directory with a year and create
#' a dataframe with selected month and year. Returns a warning for every specified year for which
#' doesn't exist a matching data file showing what year the "invalid year" is.
#'
#' @param years a vector containing a number of years
#' @return A list of data frames.
#' @examples
#' \dontrun{
#' fars_read_years(c(2013, 2014, 2015))
#' }
#' @importFrom dplyr mutate select
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select_(~ MONTH, ~ year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
