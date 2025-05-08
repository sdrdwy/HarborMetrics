library(rmarkdown)

#' Analyze ARIMA Model Forecasting Results
#'
#' Generates diagnostic metrics and model insights for ARIMA forecasting results
#' produced by \code{\link{fit_arima_forecast}}.
#'
#' @param result A list object returned by \code{fit_arima_forecast} containing:
#' \itemize{
#'   \item parameters - Model parameters including ARIMA order and coefficients
#'   \item actual_train - Actual values from training set
#'   \item train_predictions - Predicted values for training set
#'   \item actual_test - Actual values from test set
#'   \item test_predictions - Predicted values for test set
#' }
#'
#' @return Invisibly returns a list of performance metrics. Main outputs are
#' printed to console including:
#' \itemize{
#'   \item Model parameters (ARIMA order and coefficients)
#'   \item R-squared for both training and test sets
#'   \item RMSE (Root Mean Squared Error) for both sets
#' }
#'
#' @details
#' Calculates key performance indicators:
#' \itemize{
#'   \item R-squared (coefficient of determination) measuring variance explained
#'   \item RMSE (Root Mean Squared Error) in original units
#'   \item Model parameter diagnostics (ARIMA order and coefficients)
#' }
#'
#' @seealso \code{\link{fit_arima_forecast}} for generating the analysis input
#' @export
#'
#' @examples
#' \dontrun{
#' # After fitting model with fit_arima_forecast()
#' result <- fit_arima_forecast(target, exog)
#' analyze_arima_result(result)
#' 
#' # Typical console output:
#' # ARIMA(1,1,1) Coefficients:
#' #         ar1     ma1  net_contribution
#' #       0.812  -0.623            0.0041
#' # 
#' # training set R square: 0.87
#' # Training set RMSE: 1.23 hours
#' # Test set R square: 0.82
#' # Test set RMSE: 1.45 hours
#' }
analyze_arima_result <- function(result) {
  # 输出模型参数
  print(result$parameters)
  
  # 计算训练集R方
  train_r_squared <- 1 - sum((result$actual_train - result$train_predictions)^2) / sum((result$actual_train - mean(result$actual_train))^2)
  cat(sprintf("\ntraining set R square: %.2f", train_r_squared))
  # 计算训练集RMSE
  train_rmse <- sqrt(mean((result$actual_train - result$train_predictions)^2))
  cat(sprintf("\nTraining set RMSE: %.2f hours", train_rmse))

  # 计算测试集R方
  test_r_squared <- 1 - sum((result$actual_test - result$test_predictions)^2) / sum((result$actual_test - mean(result$actual_test))^2)
  cat(sprintf("\nTest set R square: %.2f", test_r_squared))
  # 计算测试集RMSE
  test_rmse <- sqrt(mean((result$actual_test - result$test_predictions)^2))
  cat(sprintf("\nTest set RMSE: %.2f hours\n", test_rmse))
}


#' Generate Comprehensive Analysis Report
#' 
#' Creates an HTML report containing data summary, model diagnostics, 
#' performance metrics, and visualizations for port operations analysis.
#'
#' @param data Cleaned port operations data (tibble/data.frame)
#' @param result Model result object from \code{\link{fit_arima_forecast}}
#' @param output_file Path for output HTML report (e.g., "analysis_report.html")
#' 
#' @return Invisibly returns the path to generated HTML report
#' 
#' @details
#' Report includes:
#' \itemize{
#'   \item Data summary statistics and structure
#'   \item ARIMA model parameters and coefficients
#'   \item Training/test set performance metrics (R², RMSE)
#'   \item Prediction visualization plots
#' }
#' 
#' @importFrom rmarkdown render
#' @importFrom knitr kable
#' @importFrom utils head
#' @export
#' 
#' @examples
#' \dontrun{
#' data <- load_port_data("data.csv") %>% clean_port_data()
#' model <- fit_arima_forecast(data$berth_duration, exog_matrix)
#' generate_report(data, model, "port_analysis.html")
#' }
generate_report <- function(data, result, output_file) {
  
  # Render report
  output_dir = getwd()
  template_path = system.file("report_template.Rmd", package = "HarborMetrics")
  rmarkdown::render(
    input = template_path,
    output_file = output_file,
    output_dir = output_dir,
    envir = new.env(parent = globalenv()),
    params = list(port_data = data, result = result)
  )
  
  # Cleanup temporary file
  # unlink(tmp_rmd)
  
  invisible(output_file)
}