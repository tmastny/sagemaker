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

# downloads s3 object into the R session
# ... passes to readr::read_delim
#' @export
read_s3 <- function(s3_path_name, delim = ",", col_names = FALSE, ...) {
  s3_components <- s3_bucket_key_extract(s3_path_name)

  io <- reticulate::import("io")
  s3 <- boto3$client('s3')

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


#' @export
s3_path <- function(...) {
  path <- file.path(..., fsep = "/") %>%
    paste0("s3://", .)

  class(path) <- c("s3_path", class(path))
  path
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
