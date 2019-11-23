#' @export
install_sagemaker <- function(method = "auto", conda = "auto") {
  reticulate::py_install(
    c("boto3", "sagemaker", "awscli"), method = method, conda = conda
  )
}
