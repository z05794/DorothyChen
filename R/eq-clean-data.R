#' Cleans source data from NOAA
#' @param df A data frame with raw data extracted from NOAA website
#' @return A data frame with cleaned date, latitude, longitude and location
#' columns
#' @examples
#' \dontrun{
#' data <- readr::read_delim("signif.txt", delim = "\t")
#' library(dplyr)
#' library(stringr)
#' library(lubridate)
#' clean_data <- eq_clean_data(df)
#' }
#' @importFrom dplyr %>% mutate select
#' @importFrom lubridate ymd
#' @importFrom stringr str_to_title str_pad
#' @export

eq_clean_data <- function(data) {
  data <- data %>%
    dplyr::mutate_(
      year_fix = ~stringr::str_pad(as.character(abs(YEAR)), width = 4,
                                   side = "left", pad = "0"),
      date_paste = ~paste(year_fix, MONTH, DAY, sep = "-"),
      datetime = ~lubridate::ymd(date_paste, truncated = 2)) %>%
    dplyr::select_(quote(-year_fix), quote(-date_paste))
  
  lubridate::year(data$DATE) <- data$YEAR
  
  data <- data %>%
    dplyr::mutate_(LATITUDE = ~as.numeric(LATITUDE),
                   LONGITUDE = ~as.numeric(LONGITUDE))
  
  data <- eq_location_clean(data)
  names(data) <- tolower(names(data))
  return(data)
}

#' Cleans earthquake location data
#'
#' @param data A data frame with raw data obtained from NOAA website
#' @return A data frame with cleaned LOCATION_NAME column
#' @examples
#' \dontrun{
#' data <- readr::read_delim("signif.txt", delim = "\t")
#' clean_data <- eq_location_clean(data)
#' }
#' @importFrom dplyr %>% mutate
#' @importFrom stringr str_replace str_trim str_to_title
#' @export
eq_location_clean <- function(data) {
  data <- data %>%
    dplyr::mutate_(LOCATION_NAME = ~LOCATION_NAME %>%
                     stringr::str_replace(paste0(COUNTRY, ":"), "") %>%
                     stringr::str_trim("both") %>%
                     stringr::str_to_title())
  names(data) <- tolower(names(data))
  return(data)
}
