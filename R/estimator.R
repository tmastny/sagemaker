#' Sagemaker Container
#'
#' @description
#' Returns the URI to the Sagemaker model container.
#'
#' Interface to \code{sagemaker$amazon$amazon_estimator$get_image_uri}.
#'
#' @param container_name Name of the Sagemaker model container, e.g. "xgboost"
#' @param repo_version Version of the model. Caution: "latest" does not
#' necessarily pull latest version.
#' @param ... Additional named arguments sent to the underlying API.
#'
#' @export
sagemaker_container <- function(container_name, repo_version = "latest", ...) {
  xgb_container <- sagemaker$amazon$amazon_estimator$get_image_uri(
    boto3$Session()$region_name,
    container_name,
    repo_version = repo_version,
    ...
  )
}

#' @rdname sagemaker_container
#' @export
sagemaker_xgb_container <- function(repo_version = "0.90-1", ...) {
  sagemaker_container("xgboost", repo_version = repo_version, ...)
}

#' Sagemaker Estimator
#'
#' Sagemaker estimator object. Interface to \code{sagemaker$estimator$Estimator}.
#'
#' @param container URI of Sagemaker model container.
#' See \link{sagemaker_container}.
#'
#' @param s3_output The S3 output path to save the model artifact.
#' See \link{s3} to construct the S3 path.
#'
#' @inheritParams sagemaker_deploy_endpoint
#' @export
sagemaker_estimator <- function(
  container,
  instance_count = 1L,
  instance_type = "ml.m4.xlarge",
  s3_output = s3(s3_bucket(), "models/"),
  ...
) {

  instance_count <- as.integer(instance_count)

  estimator <- sagemaker$estimator$Estimator(
    image_name = container,
    role = sagemaker_get_execution_role(),
    train_instance_count = instance_count,
    train_instance_type = instance_type,
    output_path = s3_output,
    ...
  )

  # TODO: check what the default hyperparameters are and
  #       set some reasonable ones
  #
  # TODO: during the `train` method, make sure
  #       the objective metric and eval metric make sense
  #       for the outcome type.
  #
  # TODO: have a wrapper to cast to int?
  if (stringr::str_detect(container, "xgboost")) {
    estimator$set_hyperparameters(
      eval_metric = "rmse",
      objective = "reg:linear",
      eta = 0.1,
      gamma = 0.0,
      min_child_weight = 1,
      num_round = 100L,
      early_stopping_rounds = 50L
    )
  }

  estimator
}

#' @rdname sagemaker_estimator
#' @export
sagemaker_xgb_estimator <- function(
  instance_count = 1L,
  instance_type = "ml.m4.xlarge",
  s3_output = s3(s3_bucket(), "models/"),
  ...
) {
  sagemaker_estimator(
    sagemaker_xgb_container(),
    instance_count = instance_count,
    instance_type = instance_type,
    output_path = s3_output,
    ...
  )
}
