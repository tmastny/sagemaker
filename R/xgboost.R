# Advice: use format generic: http://adv-r.had.co.nz/S3.html
#' @export
print.sagemaker <- function(x, ...) {
  cat(
    "Name:", x$model_name,
    "\nTuning Strategy:", x$strategy,
    "\nEvaluation Metric:", x$eval_metric, ",", x$best_eval_metric, "\n",
    "\nBest hyperparameters:\n"
  )

  cat(
    paste(
      capture.output(print(x$best_tune))[c(-1, -3)],
      collapse = "\n"
    )
  )

  invisible(x)
}

#' @export
sagemaker_tuning_job_logs <- function(sagemaker_tuner) {
  UseMethod("sagemaker_tuning_job_logs")
}

#' @export
sagemaker_tuning_job_logs.sagemaker <- function(sagemaker_tuner) {
  sagemaker_tuning_job_logs(sagemaker_tuner$tuning_job_name)
}

#' @export
sagemaker_tuning_job_logs.character <- function(sagemaker_tuner) {
  tuner_stats <- sagemaker$HyperparameterTuningJobAnalytics(sagemaker_tuner)

  tuner_stats$dataframe() %>%
    janitor::clean_names() %>%
    tibble::as_tibble()
}

#' @export
sagemaker_training_job_logs <- function(job_name) {

  # have to lookup based on job name prefix,
  # no way to get log stream name with sagemaker api
  cloudwatch <- boto3$client('logs')
  log_stream_info <- cloudwatch$describe_log_streams(
    logGroupName = "/aws/sagemaker/TrainingJobs",
    logStreamNamePrefix = job_name
  )
  log_stream_name <- log_stream_info$logStreams[[1]]$logStreamName


  job_logs <- system(
    paste0(
      "aws logs get-log-events ",
      "--log-group-name /aws/sagemaker/TrainingJobs ",
      "--log-stream-name ", log_stream_name, " ",
      "--output text"
    ),
    intern = TRUE
  )

  sage <- boto3$client('sagemaker')
  job_description <- sage$describe_training_job(TrainingJobName = job_name)

  metric_names <- job_description$FinalMetricDataList %>%
    purrr::map_chr(purrr::pluck, "MetricName")

  metric_regex <- job_description$AlgorithmSpecification$MetricDefinitions %>%
    purrr::keep(~purrr::pluck(., "Name") %in% metric_names) %>%
    purrr::map_chr(purrr::pluck, "Regex")

  job_logs %>%
    tibble::enframe(NULL, "logs") %>%
    dplyr::mutate(
      iteration = stringr::str_match(logs, ".*\\[([0-9]+)\\].*")[, 2]
    ) %>%
    dplyr::mutate(iteration = as.numeric(iteration)) %>%
    dplyr::mutate(
      !!metric_names[1] := stringr::str_match(logs, metric_regex[1])[, 2] %>%
        as.numeric()
    ) %>%
    dplyr::mutate(
      !!metric_names[2] := stringr::str_match(logs, metric_regex[2])[, 2] %>%
        as.numeric()
    ) %>%
    dplyr::select(-logs) %>%
    dplyr::filter_all(dplyr::all_vars(!is.na(.))) %>%
    dplyr::arrange(iteration)
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

#' @export
sagemaker_has_endpoint <- function(object) {
  predictor <- try_loading_endpoint(object)

  !is.null(predictor)
}

#' @export
sagemaker_delete_endpoint <- function(object) {
  predictor <- sagemaker$predictor$RealTimePredictor(
    endpoint = object$model_name
  )

  predictor$delete_endpoint()

  invisible(object)
}

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
