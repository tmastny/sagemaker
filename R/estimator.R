#' Sagemaker Container
#'
#' Returns the URI to the Sagemaker model container.
#'
#' Interface to \code{sagemaker$amazon$amazon_estimator$get_image_uri}.
#'
#' @param container_name Name of the Sagemaker model container, e.g. "xgboost"
#' @param repo_version Version of the model. Defaults to "latest".
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
sagemaker_xgb_container <- function(repo_version = "latest", ...) {
  sagemaker_container("xgboost", repo_version = repo_version, ...)
}

#' @export
sagemaker_estimator <- function(
  container,
  role = sagemaker_get_execution_role(),
  train_instance_count = 1L,
  train_instance_type = "ml.m4.xlarge",
  output_path = s3(s3_bucket(), "models/"),
  sagemaker_session = sagemaker$Session(),
  ...
) {

  train_instance_count <- as.integer(train_instance_count)

  estimator <- sagemaker$estimator$Estimator(
    image_name = container,
    role = role,
    train_instance_count = train_instance_count,
    train_instance_type = "ml.m4.xlarge",
    output_path = output_path,
    sagemaker_session = sagemaker_session,
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
  estimator$set_hyperparameters(
    eval_metric = "rmse",
    objective = "reg:linear",
    eta = 0.1,
    gamma = 0.0,
    min_child_weight = 1,
    num_round = 100L,
    early_stopping_rounds = 50L
  )

  estimator
}

#' @export
sagemaker_xgb_estimator <- function(...) {
  sagemaker_estimator(sagemaker_xgb_container(), ...)
}
