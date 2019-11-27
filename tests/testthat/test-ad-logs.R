test_that("tuning logs return", {
  skip_if_not(s3_bucket() == "sagemaker-us-east-2-495577990003")

  model <- sagemaker_attach_tuner("xgboost-191114-2052")
  model_tuning_logs <- sagemaker_tuning_job_logs(model)
  named_tuning_logs <- sagemaker_tuning_job_logs("xgboost-191114-2052")

  expect_equal(model_tuning_logs, named_tuning_logs)
  expect_true(nrow(model_tuning_logs) > 0)
  expect_equal(ncol(model_tuning_logs), 9)
})

test_that("training logs return", {
  skip_if_not(s3_bucket() == "sagemaker-us-east-2-495577990003")

  model <- sagemaker_attach_tuner("xgboost-191114-2052")
  training_logs <- sagemaker_training_job_logs(model$model_name)

  expect_equal(ncol(training_logs), 3)
  expect_true(any(stringr::str_detect(names(training_logs), "train")))
  expect_true(any(stringr::str_detect(names(training_logs), "validation")))
  expect_true(nrow(training_logs) > 0)
})
