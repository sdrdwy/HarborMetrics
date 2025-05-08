library(dplyr)
library(tidyr)

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
      across(c(net_contribution, dwt_contribution), ~ replace_na(., 0))
    ) %>%
    tidyr::drop_na() %>%
    dplyr::filter(
      berth_duration > 0,
      moor_duration >= 0,
      DailyAverageWindSpeed >= 0
    )
}


# port_data = load_port_data("data\\NewYork_dailydata.csv")
# print(length(port_data$date))
# port_data = clean_port_data(port_data)

# print(length(port_data$date))

