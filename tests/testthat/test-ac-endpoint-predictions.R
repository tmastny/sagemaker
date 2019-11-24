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

  df_pred <- tibble::enframe(predictions, NULL, "X1")

  expect_equal(df_pred, sagemaker::abalone_pred)
})

test_that("test that deploy and delete endpoint predictions work", {
  on.exit(sagemaker_delete_endpoint(model))

  model <- sagemaker_attach_tuner("xgboost-191114-2052")

  predictions <- predict(
    model, sagemaker::abalone[1:100, -1],
    deploy_endpoint = TRUE, delete_endpoint = TRUE
  )

  df_pred <- tibble::enframe(predictions, NULL, "X1")

  expect_false(sagemaker_has_endpoint(mdoel))
  expect_equal(df_pred, sagemaker::abalone_pred)
})
