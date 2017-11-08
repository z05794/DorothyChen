#' Counts the total accidents in each month and each year.
#' Create a summury table of accidents by month and year
#'
#' @param years an interger representing a year
#' @examples
#' \donttest{
#' fars_summarize_years(c(2013, 2014, 2015))
#' }
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
