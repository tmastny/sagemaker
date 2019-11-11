#' @export
sagemaker_xgb_container <- function(repo_version = "latest") {
  xgb_container <- sagemaker$amazon$amazon_estimator$get_image_uri(
    boto3$Session()$region_name,
    "xgboost",
    repo_version = repo_version
  )
}

#' @export
sagemaker_estimator <- function(
  container = sagemaker_xgb_container(),
  role = sagemaker_role(),
  train_instance_count = 1L,
  train_instance_type = "ml.m4.xlarge",
  output_path = s3_path(default_bucket(), "models/"),
  sagemaker_session = sagemaker$Session()

) {
  estimator <- sagemaker$estimator$Estimator(
    image_name = container,
    role = role,
    train_instance_count = train_instance_count,
    train_instance_type = "ml.m4.xlarge",
    output_path = output_path,
    sagemaker_session = sagemaker_session
  )

  # TODO: check what the default hyperparameters are and
  #       set some reasonable ones
  #
  # TODO: during the `train` method, make sure
  #       the objective metric and eval metric make sense
  #       for the outcome type.
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

# resamples resamples rsplit object, or a python dictionary with
#           train/test keys pointing to sagemaker$s3_input paths
#' @export
sagemaker_hyperparameter_tuner <- function(
  estimator,
  resamples,
  objective_metric_name,
  objective_type,
  hyperparameter_ranges,
  strategy = "Random",
  max_jobs = 10L,
  max_parallel_jobs = 2L,
  early_stopping_type = "Auto"
) {
  tuner <- sagemaker$tuner$HyperparameterTuner(
    estimator = estimator,
    objective_metric_name = objective_metric_name,
    objective_type = objective_type,
    hyperparameter_ranges = hyperparameter_ranges,
    strategy = strategy,
    max_jobs = max_jobs,
    max_parallel_jobs = max_parallel_jobs,
    early_stopping_type = early_stopping_type
  )

  # TODO: if given a rsplit object, need to upload it s3
  tuner$fit(resamples)
  tuner$wait()

  # TODO: attached trained tune job, rather than
  #       start from stratch

  tuner_stats <- sagemaker$HyperparameterTuningJobAnalytics(
    tuner$latest_tuning_job$job_name
  )

  tuner_df <- tuning_stats$dataframe() %>%
    janitor::clean_names()

  model_name <- tuner_df %>%
    filter(final_objective_value == min(final_objective_value)) %>%
    pull(training_job_name)

  tuning_parameter_names <- tuner$hyperparameter_ranges() %>%
    discard(is_empty) %>%
    flatten() %>%
    map_chr(pluck, "Name") %>%
    set_names(NULL)

  best_tune <- tuner_df %>%
    filter(training_job_name == model_name) %>%
    select_at(one_of(tuning_parameter_names))

  model_obj <- list(
    model_name = model_name,
    eval_metric = tuner$static_hyperparameters$eval_metric,
    best_tune = best_tune,
    metrics = tuner_df
  )

  # TODO: Constructors, validators: https://adv-r.hadley.nz/s3.html#s3-constructor

  class(model_obj) <- "sagemaker"
  model_obj
}

# Advice: use format generic: http://adv-r.had.co.nz/S3.html
# print.class <- function(x, ...) cat(format(x, ...), "\n".

print.sagemaker <- function(x, ...) {
  cat(
    "Name:", x$model_name, "\n",
    "Evaluation Metric:", x$eval_metric, "\n",
    x$best_tune
  )
}

# TODO: This is a generic function.
#       Will pull the best tuned models
#       training log if of class sagemaker,
#       or if class character will lookup
#       based on job name.
#' @export
sagemaker_training_logs <- function(job_name) {

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
    keep(~pluck(., "Name") %in% metric_names) %>%
    map_chr(pluck, "Regex")

  job_logs %>%
    tibble::enframe(NULL, "logs") %>%
    mutate(iteration = stringr::str_match(logs, ".*\\[([0-9]+)\\].*")[, 2]) %>%
    mutate(iteration = as.numeric(iteration)) %>%
    mutate(!!metric_names[1] := stringr::str_match(logs, metric_regex[1])[, 2]) %>%
    mutate(!!metric_names[2] := stringr::str_match(logs, metric_regex[2])[, 2]) %>%
    select(-logs) %>%
    filter_all(all_vars(!is.na(.))) %>%
    arrange(iteration)
}
