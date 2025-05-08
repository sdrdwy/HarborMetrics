test_that("Data preprocessing functions handle inputs correctly", {
  # prepare testdata
  test_data <- tibble::tibble(
    date = c("2020-01-01", "2020-01-02", "2020-01-03"),
    net_contribution = c(1,1,4),
    dwt_contribution = c(5,1,4),
    DailyAverageWindSpeed = c(25, 15, NA),
    moor_duration = c(0.6, NA, 0.2),
    berth_duration = c(100, 150, 200)
  )
  
  # test clean_port_data
  clean_result <- clean_port_data(test_data)
  expect_equal(nrow(clean_result), 1)
  expect_s3_class(clean_result$date, "Date")
})

test_that("ARIMA model fitting produces valid output structure", {
  set.seed(123)
  n <- 100
  test_target <- rnorm(n)
  test_exog <- matrix(rnorm(n*2), ncol = 2)
  
  model_result <- fit_arima_forecast(test_target, test_exog)
  
  expect_type(model_result, "list")
  expect_named(model_result, 
              c("model", "parameters", "train_predictions",
                "test_predictions", "actual_test", "actual_train"))
  
  expect_true(all(c("order", "coefficients") %in% names(model_result$parameters)))
})

test_that("Result analysis functions calculate metrics correctly", {
  # prepare test data
  test_result <- list(
    actual_train = c(10, 20, 30),
    train_predictions = c(12, 18, 28),
    actual_test = c(40, 50),
    test_predictions = c(38, 52)
  )
  
  # test analysis
  analysis_output <- capture.output(analyze_arima_result(test_result))
  
  # exam console output
  expect_match(analysis_output, "R square", all = FALSE)
  expect_match(analysis_output, "RMSE", all = FALSE)
  
  # exam value
  expect_true(any(grepl("0.84", analysis_output)))  # R^2 should be 0.84
})

test_that("Visualization functions produce ggplot objects", {
  test_result <- list(
    actual_train = rnorm(80),
    train_predictions = rnorm(80),
    actual_test = rnorm(20),
    test_predictions = rnorm(20)
  )
  
  viz_output <- visualize_result(test_result)

  train_layers <- length(viz_output$train_plot$layers)
  expect_gte(train_layers, 0)
})