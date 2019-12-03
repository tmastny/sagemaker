test_that("predictions work on loaded model", {
  model <- sagemaker_attach_tuner("xgboost-191114-2052")
  xgb <- sagemaker_load_model(model)

  predictions <- predict(xgb, sagemaker::abalone[1:100, -1])

  expect_equal(predictions[[1]], sagemaker::abalone_pred[[1]])
})
