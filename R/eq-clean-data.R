#' Cleans source data from NOAA
#' @param df A data frame with raw data extracted from NOAA website
#' @return A data frame with cleaned date, latitude, longitude and location
#' columns
#' @examples
#' \dontrun{
#' df <- readr::read_delim("exdata/signif.txt", delim = "\t")
#' library(dplyr)
#' library(stringr)
#' library(lubridate)
#' clean_data <- eq_clean_data(df)
#' }
#' @importFrom dplyr %>% mutate select
#' @importFrom lubridate ymd
#' @importFrom stringr str_to_title str_pad
#' @export

eq_clean_data <- function(df) {
  clean_data <- df %>%
    dplyr::select(COUNTRY, LOCATION_NAME, LATITUDE, LONGITUDE, YEAR, MONTH,
                  DAY, HOUR,EQ_MAG_ML, DEATHS, TOTAL_DEATHS, EQ_PRIMARY) %>%
    dplyr::mutate(LOCATION_NAME = stringr::str_to_title(gsub("[^;\n]+[:]", "",
                                                             LOCATION_NAME)),
                  LATITUDE = as.numeric(LATITUDE),
                  LONGITUDE = as.numeric(LONGITUDE),
                  DEATHS = as.numeric(DEATHS), 
                  year = stringr::str_pad(as.character(abs(YEAR)), width = 4,
                                          side = "left", pad = "0"),
                  date_paste = paste(year, MONTH, DAY, sep = "-"),
                  datetime = lubridate::ymd(date_paste, truncated = 2)) %>%
    dplyr::select(COUNTRY, LOCATION_NAME, LATITUDE, LONGITUDE,year,
                  EQ_MAG_ML, DEATHS, EQ_PRIMARY,  datetime)
  names(clean_data) <- tolower(names(clean_data))
  return(clean_data)
}

#' Cleans earthquake location data
#'
#' @param df A data frame with raw data obtained from NOAA website
#' @return A data frame with cleaned LOCATION_NAME column
#' @examples
#' \dontrun{
#' df <- readr::read_delim("exdata/signif.txt", delim = "\t")
#' clean_data <- eq_location_clean(df)
#' }
#' @importFrom dplyr %>% mutate
#' @importFrom stringr str_replace str_trim str_to_title

eq_location_clean <- function(df) {
  df <- df %>%
    dplyr::mutate_(LOCATION_NAME = ~LOCATION_NAME %>%
                     stringr::str_replace(paste0(COUNTRY, ":"), "") %>%
                     stringr::str_trim("both") %>%
                     stringr::str_to_title())
  names(df) <- tolower(names(df))
  return(df)
}
