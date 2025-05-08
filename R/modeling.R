
library(dplyr) 
library(tidyr)
library(R6)
library(forecast)
library(lubridate)
library(readr)


#' Fit ARIMA Forecasting Model with Train-Test Validation
#'
#' Fits an ARIMA model using automatic order selection (via forecast::auto.arima)
#' with exogenous variables, performing train-test split and returning detailed
#' model diagnostics and predictions.
#'
#' @param target A numeric vector (nx1) of the target time series to forecast.
#'   Should be a univariate time series. Missing values are not allowed.
#' @param exog A numeric matrix (nxm) of exogenous variables. Must have the same
#'   number of rows as the target vector. Factors should be converted to dummy variables.
#'
#' @return A list containing:
#' \itemize{
#'   \item model - The complete ARIMA model object from forecast::auto.arima
#'   \item parameters - List of model parameters including:
#'     \itemize{
#'       \item order - ARIMA (p,d,q) order
#'       \item coefficients - Model coefficients (AR, MA and regression terms)
#'       \item aicc - Corrected Akaike Information Criterion
#'     }
#'   \item train_predictions - Fitted values on training data
#'   \item test_predictions - Forecast values on test data
#'   \item actual_test - Actual values from test split
#'   \item actual_train - Actual values from training split
#' }
#'
#' @details
#' Key features:
#' \itemize{
#'   \item Uses 80/20 split for train/test sets
#'   \item Disables approximation for accurate model selection
#'   \item Returns both model object and prediction diagnostics
#'   \item Maintains time series order during splitting
#' }
#'
#' @importFrom forecast auto.arima forecast
#' @importFrom stats ts window coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' # Load and prepare data
#' port_data <- load_port_data("data/NewYork_dailydata.csv")
#' port_clean <- clean_port_data(port_data)
#'
#' # Prepare modeling inputs
#' target <- port_clean$berth_duration
#' exog <- as.matrix(port_clean[, c("net_contribution", "moor_duration", 
#'                                "DailyAverageWindSpeed", "berth_num")])
#'
#' # Fit model and analyze results
#' arima_result <- fit_arima_forecast(target, exog)
#' 
#' # Example analysis of results
#' plot(arima_result$actual_test, type = "l", col = "black")
#' lines(arima_result$test_predictions, col = "red")
#' legend("topright", legend = c("Actual", "Predicted"), 
#'        col = c("black", "red"), lty = 1)
#' }
fit_arima_forecast <- function(target, exog) {
  # param validatation
  if (length(target) != nrow(exog)) {
    stop("miss match in n rows of target and exogs") 
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
    order = arima_model$arma[c(1, 6, 2)],  # extract(p, d, q)
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

