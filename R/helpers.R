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

#' Sagemaker Tuning Job Logs
#'
#' Returns parameter and validation results from the tuning job.
#' The result is a data frame has one row per model, along with the trained
#' validation metric.
#'
#' @param x Either \code{sagemaker} object or tuning job name.
#'
#' @export
sagemaker_tuning_job_logs <- function(x) {
  UseMethod("sagemaker_tuning_job_logs")
}

#' @rdname sagemaker_tuning_job_logs
#' @inheritParams sagemaker_deploy_endpoint
#' @export
sagemaker_tuning_job_logs.sagemaker <- function(object) {
  sagemaker_tuning_job_logs(object$tuning_job_name)
}

#' @rdname sagemaker_tuning_job_logs
#' @param tuning_job_name Name of the tuning job, typically something
#' like \code{"xgboost-191114-2052"}.
#' @export
sagemaker_tuning_job_logs.character <- function(tuning_job_name) {
  tuner_stats <- sagemaker$HyperparameterTuningJobAnalytics(tuning_job_name)

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
#' @param job_name The training job name. Typically something like
#' \code{"xgboost-191114-2052-001-7b33b7a5"}.
#'
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
