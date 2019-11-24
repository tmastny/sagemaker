test_that("batch predictions work", {
  skip_if_not(s3_bucket() == "sagemaker-us-east-2-495577990003")

  model <- sagemaker_attach_tuner("xgboost-191114-2052")
  batch_predict(
    model,
    s3(s3_bucket(), "abalone-inference.csv"),
    s3(s3_bucket(), "abalone-transform")
  )


})
