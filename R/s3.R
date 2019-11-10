#' @export
default_bucket <- function() {
  sagemaker$Session()$default_bucket()
}

#' @export
upload_file <- function(file, bucket = default_bucket(), key) {
  s3 <- boto3$client('s3')
  s3$upload_file(file, bucket, key, extra_args, callback, config)
}

#' @export
download_file <- function(file, bucket = default_bucket(), key) {
  s3 <- boto3$client('s3')
  s3$download_file(bucket, key, file)
}
