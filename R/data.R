library(dplyr)
library(tidyr)

# Reads NewYork_dailydata.csv and converts dates.
load_port_data <- function(path){
  if(!file.exists(path)) stop("File not found:", path)
  readr::read_csv(path,show_col_types = FALSE) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::arrange(date) %>%
    tibble::as_tibble()
}

#Handels missing values and outliers
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

