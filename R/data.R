library(dplyr)
library(tidyr)

#' New York Daily Port Operations Data
#'
#' A dataset containing daily port operations metrics and weather conditions for New York.
#'
#' @format A data frame with 365 rows and 13 columns:
#' \describe{
#'   \item{date}{Date of observation (YYYY-MM-DD)}
#'   \item{net_contribution}{Net contribution to port efficiency (USD), e.g., 267894.57}
#'   \item{dwt_contribution}{Deadweight tonnage contribution (tons), e.g., 551280.4}
#'   \item{berth_duration}{Total berth duration in hours, e.g., 257.78}
#'   \item{moor_duration}{Total mooring duration in hours, e.g., 0}
#'   \item{moor_num}{Number of mooring events per day, e.g., 0}
#'   \item{berth_num}{Number of berth events per day, e.g., 7.12}
#'   \item{DailyAverageTemperature}{Daily average temperature in degrees Celsius, e.g., 5.4°C}
#'   \item{DailyAverageRelativeHumidity}{Daily average relative humidity in percent, e.g., 0%}
#'   \item{DailyAverageWindSpeed}{Daily average wind speed in miles per hour (mph), e.g., 0 mph}
#'   \item{DailyPrecipitation}{Daily precipitation in inches, e.g., 0 inches}
#'   \item{DailySnowDepth}{Daily snow depth in inches, e.g., 0 inches}
#'   \item{DailySnowfall}{Daily snowfall in inches, e.g., 0 inches}
#' }
#'
#' @source Simulated data for demonstration purposes.
#'         Data includes port operational metrics and weather measurements.
#'         Intended for use in modeling and analysis of port efficiency under varying weather conditions.
#'
#' @examples
#' # Load the dataset
#' data("NewYork_dailydata")
#'
#' # View the first few rows
#' head(NewYork_dailydata)
#'
#' # Summary statistics
#' summary(NewYork_dailydata)
"NewYork_dailydata"

#' load and preprocess port data
#' @param path Path to CSV file
#' @export 
load_port_data <- function(path){
  if(!file.exists(path)) stop("File not found:", path)
  readr::read_csv(path,show_col_types = FALSE) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::arrange(date) %>%
    tibble::as_tibble()
}

#' load and clean port data
#' @param data data loaded usikng load_port_data and wait to be cleaned
#' @export 
clean_port_data <- function(data) {
  data %>%
    dplyr::mutate(
      date = lubridate::ymd(date),
      across(c(net_contribution, dwt_contribution), ~ tidyr::replace_na(., 0))
    ) %>%
    tidyr::drop_na() %>%
    dplyr::filter(
      berth_duration > 0,
      moor_duration >= 0,
      DailyAverageWindSpeed >= 0
    )
}


# port_data = load_port_data("data/NewYork_dailydata.csv")
# print(length(port_data$date))
# port_data = clean_port_data(port_data)

# print(length(port_data$date))

