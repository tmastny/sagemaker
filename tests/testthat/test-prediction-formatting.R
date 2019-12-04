test_that("one-dimensional endpoint formatting works", {
  expected_tibble <- tibble::tibble(.pred = as.numeric(1:4))
  return_string <- "1,2,3,4"

  expect_equal(expected_tibble, format_endpoint_predictions(return_string))
})

test_that("2-dimensional endpoint formatting works", {
  expected_num_class_2 <- tibble::tibble(
    .pred_0 = as.numeric(1:2), .pred_1 = as.numeric(1:2)
  )
  return_string_num_class_2 <- "[1,1],[2,2]"

  expect_equal(
    expected_num_class_2, format_endpoint_predictions(return_string_num_class_2)
  )
})

test_that("5-dimensional endpoint formatting works", {
  v <- as.numeric(1:2)
  expected_num_class_5 <- tibble::tibble(
    .pred_0 = v, .pred_1 = v, .pred_2 = v, .pred_3 = v, .pred_4 = v
  )

  return_string_num_class_5 <- "[1,1,1,1,1],[2,2,2,2,2]"

  expect_equal(
    expected_num_class_5, format_endpoint_predictions(return_string_num_class_5)
  )
})

test_that("one-dimensional local and endpoint formatting match", {
  expected_tibble <- tibble::tibble(.pred = as.numeric(1:4))
  return_string <- "1,2,3,4"
  return_numeric <- as.numeric(1:4)

  expect_equal(expected_tibble, format_endpoint_predictions(return_string))
  expect_equal(expected_tibble, format_local_predictions(return_numeric))
})

test_that("2-dimensional local and endpoint formatting match", {
  expected_tibble <- tibble::tibble(
    .pred_0 = as.numeric(1:2), .pred_1 = as.numeric(1:2)
  )
  return_string <- "[1,1],[2,2]"
  return_numeric <- matrix(c(1, 2, 1, 2), 2, 2)

  expect_equal(expected_tibble, format_endpoint_predictions(return_string))
  expect_equal(expected_tibble, format_local_predictions(return_numeric))
})
