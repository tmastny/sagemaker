test_that("training works", {

  split <- list(
    train = s3(s3_bucket(), "abalone-train.csv"),
    validation = s3(s3_bucket(), "abalone-test.csv")
  )

  ranges <- list(
    max_depth = sagemaker_integer(3, 20),
    colsample_bytree = sagemaker_continuous(0, 1),
    subsample = sagemaker_continuous(0, 1)
  )

  tune <- sagemaker_hyperparameter_tuner(
    sagemaker_xgb_estimator(), split, ranges, max_jobs = 5
  )

  expect_s3_class(tune, "sagemaker")
})
