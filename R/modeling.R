library(ggplot2)    # 可视化
library(dplyr)      # 数据处理
library(tidyr)
library(R6)
library(forecast)
library(lubridate)
library(readr)


#' Fit arima forecast model
#' @param target a n*1 vector, output of armia
#' @param exog a n*m matrix, exogenous variables of armia
#' @export 
fit_arima_forecast <- function(target, exog) {
  # param validatation
  if (length(target) != nrow(exog)) {
    stop("目标变量与外生变量行数不一致") 
  }
  
  # create time series object
  ts_data <- ts(target)
  
  # split train/test set by 4:1
  n <- length(ts_data)
  train_size <- floor(0.8 * n)
  
  train_target <- window(ts_data, end = train_size)
  train_exog <- exog[1:train_size, , drop = FALSE]  
  
  test_target <- window(ts_data, start = train_size + 1)
  test_exog <- exog[(train_size + 1):n, , drop = FALSE]
  
  
  # ARIMA model
  arima_model <- auto.arima(
    y = train_target,
    xreg = train_exog,
    stepwise = FALSE,
    approximation = FALSE
  )
  # fit train
  train_pred <- fitted(arima_model)
  
  # test
  test_pred <- forecast(
    object = arima_model,
    h = length(test_target),
    xreg = test_exog
  )$mean
  
  # extract model parameters
  model_params <- list(
    order = arima_model$arma[c(1, 6, 2)],  # 提取(p, d, q)
    coefficients = coefficients(arima_model),
    aicc = arima_model$aicc
  )
  
  # return structure result
  return(list(
    model = arima_model,
    parameters = model_params,
    train_predictions = train_pred,
    test_predictions = test_pred,
    actual_test = test_target,
    actual_train = train_target
  ))
}


# # Usage --------------------------------------------------
# port_data = load_port_data("data/NewYork_dailydata.csv")
# print(length(port_data$date))

# port_data = clean_port_data(port_data)
# print(length(port_data$date))

# target_col = as.vector(port_data$berth_duration)
# exog_cols <- as.matrix(port_data %>% select(net_contribution, moor_duration, DailyAverageWindSpeed, berth_num))  # 外生变量（载重量和天气）

# # 调用函数
# result <- fit_arima_forecast(target_col, exog_cols)
# # 调用新函数进行分析
# analyze_arima_result(result)    
