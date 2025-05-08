#' @param result result to be analyzed
#' @export 
result_visualization <- function(result) {
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
