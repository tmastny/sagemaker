test_that("endpoint does not exist", {
  model <- sagemaker_attach_tuner("xgboost-191114-2052")

  expect_false(sagemaker_has_endpoint(model))
})

test_that("endpoint deploys", {
  model <- sagemaker_attach_tuner("xgboost-191114-2052")
  sagemaker_deploy_endpoint(model)

  expect_true(sagemaker_has_endpoint(model))
})

test_that("endpoint predictions work", {
  on.exit(sagemaker_delete_endpoint(model))

  model <- sagemaker_attach_tuner("xgboost-191114-2052")
  predictions <- predict(model, sagemaker::abalone[1:100, -1])

  expect_equal(predictions[[1]], sagemaker::abalone_pred[[1]])
})
