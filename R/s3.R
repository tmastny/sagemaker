#' Creates S3 Object Paths
#'
#' Returns properly formatted S3 object path,
#' including the \code{s3://} prefix.
#'
#' @param ... Characters vectors to combine into an S3 path.
#'
#' @examples
#' s3("my_bucket", "prefix", "object_name.csv")
#' s3(s3_bucket(), "prefix1", "prefix2", "object.csv")
#'
#' @export
s3 <- function(...) {
  path <- file.path(..., fsep = "/") %>%
    paste0("s3://", .)

  class(path) <- c("s3", class(path))
  path
}

#' Sagemaker Default S3 Bucket
#'
#' @description
#' Returns the default S3 bucket associated with the Sagemaker session.
#' Utilizes \code{sagemaker$Session()$default_bucket()}.
#'
#' Set \code{options(sagemaker.default.bucket = "bucket_name")} to use a
#' different default bucket.
#'
#' @export
s3_bucket <- function() {
  bucket <- getOption("sagemaker.default.bucket")
  if (is.null(bucket)) {
    bucket <- sagemaker$Session()$default_bucket()
  }

  bucket
}

#' Read/write \code{csv}s from S3
#'
#' @description
#' Downloads an csv file from S3 and reads it into the R session as a
#' \code{\link[tibble:tibble]{tibble::tibble()}}.
#'
#' Writes a \code{tibble} to a S3 object.
#'
#' Defaults \code{col_names} to \code{FALSE},
#' because that is \link{batch_predict} and
#' \link{sagemaker_hyperparameter_tuner} expect.
#'
#' Interface to \code{\link[readr:read_delim]{readr::read_delim()}}
#' and \code{\link[readr:format_delim]{readr::format_delim()}}
#'
#' @param s3_path A character vector that forms an S3 path to an object.
#' Use \link{s3} to construct the path.
#'
#' @inheritParams readr::read_delim
#' @inheritParams sagemaker_deploy_endpoint
#' @export
read_s3 <- function(s3_path, delim = ",", col_names = FALSE, ...) {
  s3_components <- s3_bucket_key_extract(s3_path)

  io <- reticulate::import("io")
  s3 <- boto3$client('s3')

  # https://datasciencechronicles.com.au/2017/11/12/adventures-in-python-1/
  file <- io$BytesIO()
  s3$download_fileobj(s3_components$bucket, s3_components$key, file)

  object_bytes <- file$getvalue()
  object_string <- object_bytes$decode('utf-8')

  s3_obj <- readr::read_delim(
    object_string, delim = delim, col_names = FALSE, ...
  )
  file$close()

  s3_obj
}

#' @rdname read_s3
#' @export
write_s3 <- function(x, s3_path, delim = ",", col_names = FALSE, ...) {
  s3_components <- s3_bucket_key_extract(s3_path)

  io <- reticulate::import("io")
  builtin <- reticulate::import_builtins()
  s3 <- boto3$client('s3')

  file <- io$BytesIO(builtin$bytes(
    readr::format_delim(
      x, delim = delim, col_names = col_names, ...
    ),
    "utf-8"
  ))

  s3$upload_fileobj(file, s3_components$bucket, s3_components$key)
}

#' Train/Validation Split in S3
#'
#' Returns the test/validation S3 object paths. The train and validation
#' data sets should be \code{csv}s with no column names.
#'
#' @param s3_train,s3_validation S3 paths to the train/validation datasets.
#' Construct with \link{s3}.
#'
#' @export
s3_split <- function(s3_train, s3_validation) {
  list(
    train = s3_train,
    validation = s3_validation
  )
}

s3_bucket_key_extract <- function(x) {
  stopifnot(stringr::str_sub(x, end = 5) == "s3://")

  path <- stringr::str_sub(x, 6)
  s3_path_characters <- stringr::str_split(path, "/")[[1]]

  s3_bucket <- s3_path_characters[1]
  s3_key <- s3_path_characters[-1] %>%
    stringr::str_c(collapse = "/")

  list(bucket = s3_bucket, key = s3_key)
}
