#' Deploy Sagemaker Real-time Endpoint
#'
#' Deploys a real-time Sagemaker web endpoint.
#' This process takes a few minutes. Interface to
#' \code{sagemaker$estimator$Estimator$deploy}
#'
#' @param object The \code{sagemaker} model object created by
#' \link{sagemaker_hyperparameter_tuner} or \link{sagemaker_attach_tuner}.
#'
#' @param instance_count The number of instances to run.
#'
#' @param instance_type Type of EC2 instance to run. See
#' \href{https://aws.amazon.com/sagemaker/pricing/instance-types/}{here} for
#' a list of options and pricing.
#'
#' @param wait Boolean that indicates if function should wait to return
#' until the Sagemaker process is complete.
#'
#' @param ... Additional named arguments sent to the underlying API.
#'
#' @export
sagemaker_deploy_endpoint <- function(
  object,
  instance_count = 1L,
  instance_type = "ml.t2.medium",
  wait = TRUE,
  ...
) {

  instance_count <- as.integer(instance_count)

  predict_estimator <- quietly_attach_estimator(object$model_name)

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

#' Make Predictions from Sagemaker Model
#'
#' Returns a vector of predictions from a Sagemaker real-time endpoint.
#'
#' @param new_data The \code{data.frame} or
#' \code{\link[tibble:tibble]{tibble::tibble()}} to make new predictions on.
#' Columns must be in the same order as the training data.
#' The outcome column must be excluded.
#'
#' @inheritParams sagemaker_deploy_endpoint
#' @export
predict.sagemaker <- function(object, new_data, ...) {

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

  predictions <- predictor$predict(new_data, ...)

  format_endpoint_predictions(predictions)
}

format_endpoint_predictions <- function(pred) {

  # This formats two common types of endpoint return values.
  #
  #  1-dimensional string returns
  #  (xgboost objective functions that return a class, probability, or numeric):
  #    "1,2,3,4,6,0.324"
  #
  #  n-dimensional string returns
  #  (xgboost `objective = "multi:softprob"` returns a num_class * input_rows matrix):
  #
  #     For three classes and two rows:
  #        "[0.2, 0.7, 0.1],[0.9, 0.06, 0.4]"
  #
  # The function makes them valid json arrays and converts them to a data frame.

  json_pred <- pred %>%
    as.character() %>%
    stringr::str_c("[", .data, "]") %>%
    jsonlite::parse_json()

  width <- length(json_pred[[1]]) - 1

  name <- ".pred"
  if (width > 0) {
    name <- paste0(".pred_", 0:width)
  }

  json_pred %>%
    purrr::map(purrr::set_names, nm = name) %>%
    tibble::tibble(.pred = .) %>%
    tidyr::unnest_wider(.data$.pred, names_repair = "minimal") %>%
    dplyr::mutate_all(as.numeric)
}

#' Make Predictions Locally
#'
#' This function generics predictions from the
#' \code{xgboost.core.Booster} object returned
#' from \code{\link{sagemaker_load_model}}.
#'
#' @inheritParams sagemaker_container
#' @inheritParams predict.sagemaker
#' @export
predict.xgboost.core.Booster <- function(object, new_data, ...) {
  xgb <- reticulate::import("xgboost")
  blt <- reticulate::import_builtins()

  new_data <- xgb$DMatrix(new_data)

  # TODO: I thought type would be as simple as
  #       object$predict_proba, it's not available:
  #       https://github.com/awslabs/amazon-sagemaker-examples/issues/479
  #
  #       So some objective metrics only support class or probability.
  #       For example, softmax only outputs class, while
  #       softprob outputs probability.
  #
  #       To do this right, I would need to force
  #       the user to select the probability outputs,
  #       and convert them on the backend...

  # parameters from Sagemaker xgboost container for consistency:
  # https://github.com/aws/sagemaker-xgboost-container/blob/fc364c7c844859de1852acd526111ee22ac8e393/src/sagemaker_xgboost_container/algorithm_mode/serve.py#L119-L121
  #
  # `best_ntree_limit` ensures predictions are done with early stopping:
  # https://stackoverflow.com/a/51985193/6637133
  pred <- object$predict(
    new_data,
    ntree_limit = blt$getattr(object, "best_ntree_limit", 0L),
    validate_features = FALSE,
    ...
  )

  format_local_predictions(pred)
}

format_local_predictions <- function(pred) {
  if (length(dim(pred)) > 1) {
    pred_df <- tibble::as_tibble(pred) %>%
      dplyr::mutate_all(as.numeric)

    names(pred_df) <- paste0(".pred_", 0:(dim(pred)[2] - 1))

    return(pred_df)
  }

  tibble::enframe(as.numeric(pred), NULL, ".pred")
}

#' Batch Predictions from Sagemaker Model
#'
#' Runs a batch of predictions on Sagemaker model.
#' The input is a csv file on S3, and predictions are saved
#' as a csv on S3.
#'
#' @param s3_input The S3 path to the input object.
#' The object must be a \code{csv}, with no column names.
#' The \code{csv} must not contain the outcome column.
#' Use \link{s3} to construct the path.
#'
#' @param s3_output The S3 prefix for the output object or objects.
#'
#'
#' @return S3 output path.
#'
#' The format is
#' \code{s3://[s3_output]/[s3_input_object_name].out}
#'
#' @inheritParams sagemaker_deploy_endpoint
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

  predict_estimator <- quietly_attach_estimator(object$model_name)

  predict_transformer <- predict_estimator$transformer(
    instance_count = instance_count,
    instance_type = instance_type,
    output_path = s3_output,
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

quietly_attach_estimator <- function(training_job_name) {
  reticulate::py_capture_output({
    estimator <- sagemaker$estimator$Estimator$attach(
      training_job_name = training_job_name
    )
  })

  estimator
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
