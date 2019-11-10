#' @export
install_sagemaker <- function(method = "auto", conda = "auto") {
  reticulate::py_install(
    c("boto3", "sagemaker"), method = method, conda = conda
  )
}

