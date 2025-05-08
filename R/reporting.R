#' Fit arima forecast model
#' @param result result to be analyzed
#' @export 
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
  cat(sprintf("\nTest set RMSE: %.2f hours", test_rmse))
}

generate_report <- function(data, output_file) {

}




