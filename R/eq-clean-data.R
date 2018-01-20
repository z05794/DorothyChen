#' Cleans source data from NOAA
#' @param df A data frame with raw data extracted from NOAA website
#' @return A data frame with cleaned date, latitude, longitude and location
#' columns
#' @examples
#' \dontrun{
#' raw_data <- readr::read_delim("exdata/signif.txt", delim = "\t")
#' library(dplyr)
#' library(stringr)
#' library(lubridate)
#' clean_data <- eq_clean_data(raw_data)
#' }
#' @importFrom dplyr %>% mutate select
#' @importFrom lubridate ymd
#' @importFrom stringr str_to_title str_pad
#' @export

eq_clean_data <- function(df) {
  clean_data <- df %>%
    dplyr::select(COUNTRY, LOCATION_NAME, LATITUDE, LONGITUDE, YEAR, MONTH,
                  DAY, HOUR,EQ_MAG_ML, DEATHS, TOTAL_DEATHS, EQ_PRIMARY) %>%
    dplyr::mutate(LOCATION_NAME = str_to_title(gsub("[^;\n]+[:]", "", 
                                                    LOCATION_NAME)),
                  LATITUDE = as.numeric(LATITUDE),
                  LONGITUDE = as.numeric(LONGITUDE),
                  DEATHS = as.numeric(DEATHS), 
                  year = str_pad(as.character(abs(YEAR)), width = 4, 
                                 side = "left", pad = "0"),
                  date_paste = paste(year, MONTH, DAY, sep = "-"),
                  datetime = ymd(date_paste, truncated = 2)) %>%
    dplyr::select(COUNTRY, LOCATION_NAME, LATITUDE, LONGITUDE, datetime, year,
                  EQ_MAG_ML, DEATHS, TOTAL_DEATHS, EQ_PRIMARY)
  names(clean_data) <- tolower(names(clean_data))
  return(clean_data)
}
