test_that("predictions work on loaded model", {
  model <- sagemaker_attach_tuner("xgboost-191114-2052")
  xgb <- sagemaker_load_model(model)

  predictions <- predict(xgb, sagemaker::abalone[1:100, -1])

  expect_equal(predictions[[1]], sagemaker::abalone_pred[[1]])
})

test_that("local and endpoint predictions agree", {
  class_model <- sagemaker_attach_tuner("sagemaker-xgboost-191201-1356")
  prob_model <- sagemaker_attach_tuner("sagemaker-xgboost-191201-2049")

  skip_if_not(sagemaker_has_endpoint(class_model))
  skip_if_not(sagemaker_has_endpoint(prob_model))

  class_model_local <- sagemaker_load_model(class_model)
  prob_model_local <- sagemaker_load_model(prob_model)

  new_data <- abalone_class()[, -1]
  class_local_pred <- predict(class_model_local, new_data)
  class_model_pred <- predict(class_model, new_data)

  prob_local_pred <- predict(prob_model_local, new_data) %>%
    dplyr::mutate_all(round, digits = 4)
  prob_model_pred <- predict(prob_model, new_data) %>%
    dplyr::mutate_all(round, digits = 4)

  expect_equal(class_local_pred, class_model_pred)
  expect_equal(prob_local_pred, prob_model_pred)
})

