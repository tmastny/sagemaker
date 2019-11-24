test_that("batch predictions work", {
  skip_if_not(s3_bucket() == "sagemaker-us-east-2-495577990003")
  s3_batch_test_path <- s3(s3_bucket(), "tests", "batch-pred-test.csv")

  model <- sagemaker_attach_tuner("xgboost-191114-2052")
  output_path <- batch_predict(
    model,
    s3_batch_test_path,
    s3(s3_bucket(), "tests", "batch-pred-test-output")
  )

  expect_equal(read_s3(output_path), sagemaker::abalone_pred)
})
