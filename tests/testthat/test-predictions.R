test_that("batch predictions work", {
  model <- sagemaker_attach_tuner("xgboost-191114-2052")
  batch_predict(
    model,
    s3(s3_bucket(), "abalone-inference.csv"),
    s3(s3_bucket(), "abalone-transform")
  )


})
