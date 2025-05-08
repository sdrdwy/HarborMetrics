#' Fit arima forecast model
#' @param result result to be analyzed
#' @export 
analyze_arima_result <- function(result) {
  # 输出模型参数
  print(result$parameters)
  
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

#RMarkdown summary of key insights
generate_report = function(data,output_file){


}