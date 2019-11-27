try_loading_endpoint <- function(object) {
  tryCatch(
    sagemaker$predictor$RealTimePredictor(endpoint = object$model_name),
    error = function(condition) {
      if (!stringr::str_detect(condition$message, "Could not find endpoint")) {
        stop(condition)
      }

      NULL
    }
  )
}

#' Deploy Sagemaker Endpoint
#' @export
sagemaker_deploy_endpoint <- function(
  object,
  instance_count = 1L,
  instance_type = "ml.t2.medium",
  wait = TRUE,
  ...
) {

  instance_count <- as.integer(instance_count)

  predict_estimator <- sagemaker$estimator$Estimator$attach(
    training_job_name = object$model_name
  )

  predict_estimator$deploy(
    initial_instance_count = instance_count,
    instance_type = instance_type,
    wait = wait,
    ...
  )

  invisible(object)
}

#' @rdname sagemaker_deploy_endpoint
#' @export
sagemaker_has_endpoint <- function(object) {
  predictor <- try_loading_endpoint(object)

  !is.null(predictor)
}

#' @rdname sagemaker_deploy_endpoint
#' @export
sagemaker_delete_endpoint <- function(object) {
  predictor <- sagemaker$predictor$RealTimePredictor(
    endpoint = object$model_name
  )

  predictor$delete_endpoint()

  invisible(object)
}

#' @export
predict.sagemaker <- function(object, new_data) {

  predictor <- try_loading_endpoint(object)

  if (is.null(predictor)) {
    stop(
      "No existing endpoint to deploy to. ",
      "Endpoint should have the same name as the model."
    )
  }

  predictor$content_type <- "text/csv"
  predictor$serializer <- sagemaker$predictor$csv_serializer

  new_data <- as.matrix(new_data)
  dimnames(new_data)[[2]] <- NULL

  predictions <- predictor$predict(new_data)

  predictions <- predictions %>%
    stringr::str_split(pattern = ",", simplify = TRUE) %>%
    as.numeric()

  if (delete_endpoint) {
    predictor$delete_endpoint()
  }

  predictions
}

# returns the s3_output_path
#' @export
batch_predict <- function(
  object,
  s3_input,
  s3_output,
  instance_count = 1L,
  instance_type = "ml.c4.xlarge",
  wait = TRUE
) {

  instance_count <- as.integer(instance_count)

  predict_estimator <- sagemaker$estimator$Estimator$attach(
    training_job_name = object$model_name
  )

  predict_transformer <- predict_estimator$transformer(
    instance_count = instance_count,
    instance_type = instance_type,
    output_path = s3_output_path,
    assemble_with = 'Line'
  )

  predict_transformer$transform(
    s3_input,
    content_type = "text/csv",
    split_type = "Line",
    wait = wait,
    logs = FALSE
  )

  # TODO: make s3 generic so it knows how
  #       to take a s3 object and not double
  #       transform it like "s3://s3://".
  #       I think there are some examples in Shiny
  #       or htmltools.
  s3_predictions_path <- paste0(
    predict_transformer$output_path, "/",
    paste0(basename(s3_input), ".out")
  )

  s3_predictions_path
}

# https://docs.aws.amazon.com/sagemaker/latest/dg/xgboost-tuning.html
objective_metric_type <- function(objective_metric) {
  objective_metric_optimization <- list(
    `validation:accuracy` = "Maximize",
    `validation:auc` = "Maximize",
    `validation:error` = "Minimize",
    `validation:f1` = "Maximize",
    `validation:logloss` = "Minimize",
    `validation:mae` = "Minimize",
    `validation:map` = "Maximize",
    `validation:merror` = "Minimize",
    `validation:mlogloss` = "Minimize",
    `validation:mse` = "Minimize",
    `validation:ndcg` = "Maximize",
    `validation:rmse` = "Minimize"
  )

  objective_metric_optimization[[objective_metric]]
}

objective_metric_name <- function(estimator_eval_metric) {
  objective_metrics <- c(
    "validation:accuracy",
    "validation:auc",
    "validation:error",
    "validation:f1",
    "validation:logloss",
    "validation:mae",
    "validation:map",
    "validation:merror",
    "validation:mlogloss",
    "validation:mse",
    "validation:ndcg",
    "validation:rmse"
  )

  objective_metrics %>%
    purrr::keep(~stringr::str_detect(., estimator_eval_metric))
}
