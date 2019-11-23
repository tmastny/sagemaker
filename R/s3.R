# use options(sagemaker.default.bucket = "bucket_name")
# or will default to sagemaker$Session()$default_bucket()
#' @export
s3_bucket <- function() {
  bucket <- getOption("sagemaker.default.bucket")
  if (is.null(bucket)) {
    bucket <- sagemaker$Session()$default_bucket()
  }

  bucket
}

#' @export
upload_file <- function(file, s3_path) {
  s3 <- boto3$client('s3')

  s3_components <- s3_bucket_key_extract(s3_path)
  s3$upload_file(file, s3_components$bucket, s3_components$key)
}


#' @export
download_file <- function(s3_path, file) {
  system(
    paste0(
      "aws s3 cp ",
      s3_path, " ",
      file
    )
  )
}

# downloads s3 object into the R session
# ... passes to readr::read_delim
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

# ... to format_csv
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




#' @export
s3 <- function(...) {
  path <- file.path(..., fsep = "/") %>%
    paste0("s3://", .)

  class(path) <- c("s3", class(path))
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
