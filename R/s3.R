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
download_file <- function(s3_path_name, file_name) {
  # TODO: is it possible to download to R session, without
  #       going through a file?

  system(
    paste0(
      "aws s3 cp ",
      s3_path_name(), " ",
      file_name
    )
  )
}

#' @export
s3_path <- function(...) {
  path <- file.path(..., fsep = "/") %>%
    paste0("s3://", .)

  class(path) <- c("s3_path", class(path))
  path
}
