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
      utils::capture.output(print(x$best_tune))[c(-1, -3)],
      collapse = "\n"
    )
  )

  invisible(x)
}

#' Sagemaker Tuning Job Logs
#'
#' Returns parameter and validation results from the tuning job.
#' The result is a data frame has one row per model, along with the trained
#' validation metric.
#'
#' @param x Either the \code{sagemaker} model object created by
#' \link{sagemaker_hyperparameter_tuner} or \link{sagemaker_attach_tuner}
#' or the tuning job name.
#'
#' @export
sagemaker_tuning_job_logs <- function(x) {
  UseMethod("sagemaker_tuning_job_logs")
}

#' @rdname sagemaker_tuning_job_logs
#' @export
sagemaker_tuning_job_logs.sagemaker <- function(x) {
  sagemaker_tuning_job_logs(x$tuning_job_name)
}

#' @rdname sagemaker_tuning_job_logs
#' @export
sagemaker_tuning_job_logs.character <- function(x) {
  tuner_stats <- sagemaker$HyperparameterTuningJobAnalytics(x)

  tuner_stats$dataframe() %>%
    janitor::clean_names() %>%
    tibble::as_tibble()
}

#' Sagemaker Training Job Logs
#'
#' Returns the train/evaluation metrics per round of training for a
#' training job. Typically associated with \code{nround}s for xgboost
#' or epochs for neural networks.
#'
#' @param training_job_name The training job name. Typically something like
#' \code{"xgboost-191114-2052-001-7b33b7a5"}.
#'
#' @export
sagemaker_training_job_logs <- function(training_job_name) {

  # have to lookup based on job name prefix,
  # no way to get log stream name with sagemaker api
  cloudwatch <- boto3$client('logs')
  log_stream_info <- cloudwatch$describe_log_streams(
    logGroupName = "/aws/sagemaker/TrainingJobs",
    logStreamNamePrefix = training_job_name
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
  job_description <- sage$describe_training_job(TrainingJobName = training_job_name)

  metric_names <- job_description$FinalMetricDataList %>%
    purrr::map_chr(purrr::pluck, "MetricName")

  metric_regex <- job_description$AlgorithmSpecification$MetricDefinitions %>%
    purrr::keep(~purrr::pluck(., "Name") %in% metric_names) %>%
    purrr::map_chr(purrr::pluck, "Regex")

  job_logs %>%
    tibble::enframe(NULL, "logs") %>%
    dplyr::mutate(
      iteration = stringr::str_match(.data$logs, ".*\\[([0-9]+)\\].*")[, 2]
    ) %>%
    dplyr::mutate(iteration = as.numeric(.data$iteration)) %>%
    dplyr::mutate(
      !!metric_names[1] := stringr::str_match(.data$logs, metric_regex[1])[, 2] %>%
        as.numeric()
    ) %>%
    dplyr::mutate(
      !!metric_names[2] := stringr::str_match(.data$logs, metric_regex[2])[, 2] %>%
        as.numeric()
    ) %>%
    dplyr::select(-.data$logs) %>%
    dplyr::filter_all(dplyr::all_vars(!is.na(.))) %>%
    dplyr::arrange(.data$iteration)
}

#' Loads Model Artifact
#'
#' Loads the model artifact in the current R session.
#' Currently only supports xgboost models.
#'
#' @details
#' xgboost models require the xgboost Python package.
#' See \link{sagemaker_install_xgboost} to download.
#'
#' xgboost models return a \code{xgboost.core.Booster}
#' from the xgboost Python package. See
#' \code{\link{predict.xgboost.core.Booster}}.
#'
#' @param x Either the \code{sagemaker} model object created by
#' \link{sagemaker_hyperparameter_tuner} or \link{sagemaker_attach_tuner}
#' or the training job name.
#'
#' @export
sagemaker_load_model <- function(x) {
  UseMethod("sagemaker_load_model")
}

#' @rdname sagemaker_load_model
#' @export
sagemaker_load_model.sagemaker <- function(x) {
  sagemaker_load_model(x$model_name)
}

#' @rdname sagemaker_load_model
#' @export
sagemaker_load_model.character <- function(x) {
  # TODO: long-term, I think this might need to be a
  #       generic based on the type of estimator
  #       (e.g. linear, xgboost, etc.)

  model_path <- model_artifact_s3_path(x)
  model_s3_components <- s3_bucket_key_extract(model_path)

  io      <- reticulate::import("io")
  boto3   <- reticulate::import("boto3")
  tarfile <- reticulate::import("tarfile")
  pkl     <- reticulate::import("pickle")
  xgb     <- reticulate::import("xgboost")

  bytes_container <- io$BytesIO()

  s3 <- boto3$client('s3')
  s3$download_fileobj(
    model_s3_components$bucket, model_s3_components$key, bytes_container
  )

  raw_bytes <- bytes_container$getvalue()
  model_tar <- tarfile$open(fileobj = io$BytesIO(raw_bytes), mode = 'r:gz')

  model_pickle <- model_tar$extractfile("xgboost-model")

  pkl$load(model_pickle)
}

#' Downloads Model Artifact
#'
#' Downloads model artifact from S3.
#'
#' @param path A file path.
#'
#' @inheritParams sagemaker_load_model
#' @export
sagemaker_download_model <- function(x, path) {
  UseMethod("sagemaker_download_model")
}

#' @rdname sagemaker_download_model
#' @export
sagemaker_download_model.sagemaker <- function(x, path) {
  sagemaker_download_model(x$model_name)
}

#' @rdname sagemaker_download_model
#' @export
sagemaker_download_model.character <- function(x, path) {
  model_path <- model_artifact_s3_path(x)
  system(
    paste0(
      "aws s3 cp ",
      model_path, " ",
      path
    )
  )
}

model_artifact_s3_path <- function(job_name) {
  job <- quietly_attach_estimator(job_name)
  job$model_data
}
