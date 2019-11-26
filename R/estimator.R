#' @export
sagemaker_xgb_container <- function(repo_version = "latest", ...) {
  xgb_container <- sagemaker$amazon$amazon_estimator$get_image_uri(
    boto3$Session()$region_name,
    "xgboost",
    repo_version = repo_version,
    ...
  )
}

# requires `pip install awscli`
# guide: https://www.viget.com/articles/set-up-aws-cli-and-download-your-s3-files-from-the-command-line/
#' @export
sagemaker_get_execution_role <- function(
  var_name = "role_arn", profile_name = "sagemaker"
) {

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
    paste0("aws configure get ", var_name, " --profile ", profile_name),
    intern = TRUE
  )
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
