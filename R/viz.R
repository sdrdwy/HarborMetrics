library(ggplot2)  

#' Visualize ARIMA Model Training and Testing Predictions
#'
#' Generates time series plots comparing actual vs predicted values for both
#' training and testing sets from ARIMA model results.
#'
#' @param result A list object returned by \code{\link{fit_arima_forecast}} containing:
#' \itemize{
#'   \item actual_train - Actual values from training set (numeric vector)
#'   \item train_predictions - Model predictions on training set (numeric vector)
#'   \item actual_test - Actual values from test set (numeric vector)
#'   \item test_predictions - Model predictions on test set (numeric vector)
#' }
#'
#' @return Invisibly returns a list containing:
#' \itemize{
#'   \item train_plot - ggplot object of training set visualization
#'   \item test_plot - ggplot object of test set visualization
#' }
#' Plots are automatically printed to the active graphics device.
#'
#' @details
#' Creates two time series plots using \code{ggplot2} semantics:
#' \itemize{
#'   \item Training plot shows model fitting performance
#'   \item Testing plot shows out-of-sample forecasting performance
#'   \item Solid line represents actual values
#'   \item Dashed line represents model predictions
#' }
#'
#' @seealso
#' \code{\link{fit_arima_forecast}} for generating model results
#' \code{\link{analyze_arima_result}} for quantitative diagnostics
#' 
#' @importFrom ggplot2 autoplot autolayer labs
#' @importFrom stats ts
#' @export
#'
#' @examples
#' \dontrun{
#' # Fit model first
#' model_result <- fit_arima_forecast(target, exog)
#' 
#' # Generate visualizations
#' visualize_result(model_result)
#' 
#' # Access plots directly
#' plots <- visualize_result(model_result)
#' plots$train_plot + ggplot2::ggtitle("Custom Training Plot Title")
#' }
visualize_result <- function(result) {
  # 可视化训练集预测
  train_plot <- autoplot(ts(result$actual_train)) +
    autolayer(ts(result$train_predictions)) +
    labs(title = "ARIMA model training set predicts the effect",
         x = "Time index", 
         y = "Mooring time (hours)")
  print(train_plot)
  
  # 可视化测试集预测
  test_plot <- autoplot(ts(result$actual_test)) +
    autolayer(ts(result$test_predictions)) +
    labs(title = "ARIMA model test set prediction effect",
         x = "Time index", 
         y = "Mooring time (hours)")
  print(test_plot)
}
