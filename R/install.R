#' @export
sagemaker_install <- function(method = "auto", conda = "auto") {
  reticulate::py_install(
    c("boto3", "sagemaker", "awscli"), method = method, conda = conda
  )
}

#' @export
sagemaker_save_execution_role <- function(role_arn, profile_name = "sagemaker") {
  stopifnot(.Platform$OS.type == "unix")

  profile_name <- "test"
  role_arn <- "role"

  system(
    paste0(
      "echo '\n[profile ", profile_name, "]\n",
      "role_arn = ", role_arn, "' >> ~/.aws/config"
    )
  )
}
