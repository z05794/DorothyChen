#' Create a filename
#'
#' Make a new file name reflecting a given year
#'
#' @param year a character string representing a year
#' @return A character string matching the complete filename
#' @examples
#' \donttest{
#' make_filename(2014)
#' }
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
