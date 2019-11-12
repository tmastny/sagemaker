#' @export
default_bucket <- function() {
  sagemaker$Session()$default_bucket()
}

#' @export
upload_file <- function(file, bucket = default_bucket(), key) {
  s3 <- boto3$client('s3')
  s3$upload_file(file, bucket, key)
}

# TODO:
# needs to be able to take a single path
# I think the path should be first
#' @export
download_file <- function(file, bucket = default_bucket(), key) {
  s3 <- boto3$client('s3')
  s3$download_file(bucket, key, file)
}

#' @export
s3_path <- function(...) {
  file.path(..., fsep = "/") %>%
    paste0("s3://", .)
}
