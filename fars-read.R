#' Read in a data set
#'
#' Reads a data file from the current working directory and converts it to a
#' dataframe if a data file exist. Returns an error if the file doesn't exists, and
#' show an error message "file (file name) does not exist"
#'
#' @param filename a text with ".csv" or ".csv.bz2" as an extension
#' @return a dataframe of raw data
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' }
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
