#' @export
default_bucket <- function() {
  sagemaker$Session()$default_bucket()
}

#' @export
upload_file <- function(file, bucket = default_bucket(), key) {
  s3 <- boto3$client('s3')
  s3$upload_file(file, bucket, key)
}

#' @export
download_file <- function(file, bucket = default_bucket(), key) {
  s3 <- boto3$client('s3')
  s3$download_file(bucket, key, file)
}

# use sagemaker.get_execution_role() if on sagemaker notebook instance
# requires `pip install awscli`
#' @export
sagemaker_role <- function(
  var_name = "role_arn", profile_name = "sagemaker"
) {
  system(
    paste0("aws configure get ", var_name, " --profile ", profile_name),
    intern = TRUE
  )
}

#' @export
s3_path <- function(...) {
  file.path(..., fsep = "/") %>%
    paste0("s3://", .)
}
