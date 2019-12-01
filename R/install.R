#' Installs Sagemaker Dependencies
#'
#' Installs the Python package dependencies boto3, sagemaker, and awscli.
#' Installs the Python xgboost package,
#' for using downloaded model artifacts.
#' TODO: link to sagemaker_download_artifact
#'
#' Interface to \code{\link[reticulate:py_install]{reticulate::py_install()}}.
#'
#' @inheritParams sagemaker_deploy_endpoint
#' @export
sagemaker_install <- function(...) {
  reticulate::py_install(c("boto3", "sagemaker", "awscli"), ...)
}

#' @rdname sagemaker_install
#' @export
sagemaker_install_xgboost <- function(...) {
  reticulate::py_install("xgboost", ...)
}

#' Sagemake Execution Role
#'
#' @description
#' Functions that read and write the local Sagemaker execution role.
#' The role will be saved locally in \code{~/.aws/config}.
#'
#' Use \code{options(sagemaker.default.role.profile = "profile_name")} to
#' change the default profile name.
#'
#' @details
#' Requires
#' \href{https://www.viget.com/articles/set-up-aws-cli-and-download-your-s3-files-from-the-command-line/}{awscli}.
#'
#' The Sagemaker ARN has the following form:
#'
#' \code{
#' arn:aws:iam::(account_number):role/service-role/(SageMakerExecutionRole)
#' }
#'
#' @param profile_name Name of profile to save in the \code{config} file.
#' @param role_arn The ARN of the Sagemaker execution role. See below for
#' an example.
#'
#' @export
sagemaker_save_execution_role <- function(role_arn, profile_name = "sagemaker") {
  stopifnot(.Platform$OS.type == "unix")

  option_profile_name <- getOption("sagemaker.default.role.profile")
  if (!is.null(option_profile_name)) {
    profile_name <- option_profile_name
  }

  system(
    paste0(
      "echo '\n[profile ", profile_name, "]\n",
      "role_arn = ", role_arn, "' >> ~/.aws/config"
    )
  )
}

#' @rdname sagemaker_save_execution_role
#' @export
sagemaker_get_execution_role <- function(profile_name = "sagemaker") {

  option_profile_name <- getOption("sagemaker.default.role.profile")
  if (!is.null(option_profile_name)) {
    profile_name <- option_profile_name
  }

  role <- tryCatch(
    sagemaker$get_execution_role(),
    error = function(condition) {
      message(
        "\n\nYou are not in a sagemaker hosted notebook.\n",
        "Pulling local role in ~/.aws/config"
      )

      NULL
    }
  )

  if (!is.null(role)) {
    return(role)
  }

  system(
    paste0("aws configure get role_arn --profile ", profile_name),
    intern = TRUE
  )
}
