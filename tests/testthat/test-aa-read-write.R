local_comparison_data <- sagemaker::abalone[1:100, ] %>%
  magrittr::set_names(paste0("X", 1:11))

test_that("reading from s3 works", {
  s3_read_path <- s3(s3_bucket(), "read-write-test.csv")
  expect_equal(read_s3(s3_read_path) , local_comparison_data)
})

test_that("writing to s3 works", {
  s3_write_path <- s3(s3_bucket(), "write-read-test.csv")
  write_s3(local_comparison_data, s3_write_path)

  expect_equal(read_s3(s3_write_path), local_comparison_data)
})
