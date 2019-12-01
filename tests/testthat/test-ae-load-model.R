test_that("loading model locally works", {
  model <- sagemaker_attach_tuner("xgboost-191114-2052")
  xgb <- sagemaker_load_model(model)

  predictions <- predict(xgb, sagemaker::abalone[1:100, -1])

  expect_equal(predictions, sagemaker::abalone_pred[[1]])
})
