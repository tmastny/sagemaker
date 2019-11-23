#' @export
sagemaker_xgb_container <- function(repo_version = "latest") {
  xgb_container <- sagemaker$amazon$amazon_estimator$get_image_uri(
    boto3$Session()$region_name,
    "xgboost",
    repo_version = repo_version
  )
}

#' @export
sagemaker_save_execution_role <- function(role_arn, profile_name = "sagemaker") {
  stopifnot(.Platform$OS.type == "unix")

  profile_name <- "test"
  role_arn <- "role"

  system(
    paste0(
      "echo '\n[profile ", profile_name, "]\n",
      "role_arn = ", role_arn, "' >> ~/.aws/config"
    )
  )
}

# requires `pip install awscli`
#' @export
sagemaker_get_execution_role <- function(
  var_name = "role_arn", profile_name = "sagemaker"
) {

  role <- NULL

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
  output_path = s3_path(default_bucket(), "models/"),
  sagemaker_session = sagemaker$Session()

) {

  train_instance_count <- as.integer(train_instance_count)

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

# resamples resamples rsplit object, or a python dictionary with
#           train/test keys pointing to sagemaker$s3_input paths
#' @export
sagemaker_hyperparameter_tuner <- function(
  estimator,
  split,
  hyperparameter_ranges,
  strategy = "Random",
  max_jobs = 10L,
  max_parallel_jobs = 2L,
  early_stopping_type = "Auto"
) {

  max_jobs <- as.integer(max_jobs)
  max_parallel_jobs <- as.integer(max_parallel_jobs)

  objective_metric <- objective_metric_name(estimator$hyperparam_dict$eval_metric)

  tuner <- sagemaker$tuner$HyperparameterTuner(
    estimator = estimator,
    objective_metric_name = objective_metric,
    objective_type = objective_metric_type(objective_metric),
    hyperparameter_ranges = hyperparameter_ranges,
    strategy = strategy,
    max_jobs = max_jobs,
    max_parallel_jobs = max_parallel_jobs,
    early_stopping_type = early_stopping_type
  )

  # TODO: if given a rsplit object, need to upload it s3

  stopifnot("train" %in% names(split))
  stopifnot("validation" %in% names(split))

  split_dict <- reticulate::dict(
    train = sagemaker$s3_input(split$train, content_type = "text/csv"),
    validation = sagemaker$s3_input(split$validation, content_type = "text/csv")
  )

  tuner$fit(split_dict)
  tuner$wait()

  sagemaker_attach_tuner(tuner$latest_tuning_job$job_name)
}


#' @export
sagemaker_attach_tuner <- function(tuning_job_name) {

  tuner_df <- sagemaker_tuning_job_logs(tuning_job_name)

  model_name <- tuner_df %>%
    dplyr::filter(final_objective_value == min(final_objective_value)) %>%
    dplyr::pull(training_job_name)

  tuning_parameter_names <- tuner$hyperparameter_ranges() %>%
    purrr::discard(purrr::is_empty) %>%
    purrr::flatten() %>%
    purrr::map_chr(purrr::pluck, "Name") %>%
    purrr::set_names(NULL)

  best_tune <- tuner_df %>%
    dplyr::filter(training_job_name == model_name) %>%
    dplyr::select(dplyr::one_of(tuning_parameter_names))

  best_eval_metric <- tuner_df %>%
    dplyr::filter(training_job_name == model_name) %>%
    dplyr::pull(final_objective_value)

  tuner <- sagemaker$tuner$HyperparameterTuner$attach(tuning_job_name)

  model_obj <- list(
    model_name = model_name,
    tuning_job_name = tuning_job_name,
    eval_metric = tuner$estimator$hyperparam_dict$eval_metric,
    strategy = tuner$strategy,
    best_eval_metric = best_eval_metric,
    best_tune = best_tune,
    metrics = tuner_df
  )

  # TODO: Constructors, validators: https://adv-r.hadley.nz/s3.html#s3-constructor

  class(model_obj) <- "sagemaker"
  model_obj
}


# Advice: use format generic: http://adv-r.had.co.nz/S3.html
# print.class <- function(x, ...) cat(format(x, ...), "\n".
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

sagemaker_tuning_job_logs.sagemaker <- function(sagemaker_tuner) {
  sagemaker_tuning_job_logs(sagemaker_tuner$tuning_job_name)
}

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
      !!metric_names[1] := stringr::str_match(logs, metric_regex[1])[, 2]
    ) %>%
    dplyr::mutate(
      !!metric_names[2] := stringr::str_match(logs, metric_regex[2])[, 2]
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

  object
}

#' @export
sagemaker_deploy_endpoint <- function(
  object,
  instance_count = 1L,
  instance_type = "ml.t2.medium"
) {

  instance_count <- as.integer(instance_count)

  predict_estimator <- sagemaker$estimator$Estimator$attach(
    training_job_name = object$model_name
  )

  predict_estimator$deploy(
    initial_instance_count = instance_count,
    instance_type = instance_type
  )

  object
}

#' @export
predict.sagemaker <- function(
  object,
  new_data,
  instance_count = 1L,
  instance_type = "ml.t2.medium",
  deploy_endpoint = FALSE,
  delete_endpoint = FALSE
) {

  predictor <- try_loading_endpoint(object)

  if (deploy_endpoint & is.null(predictor)) {
    message("Deploying Sagemaker endpoint. This will take a few minutes...")

    sagemaker_deploy_endpoint(object, instance_count, instance_type)
    predictor <- try_loading_endpoint(object)
  } else if (is.null(predictor)) {
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
  s3_input_path,
  s3_output_path,
  instance_count = 1L,
  instance_type = "ml.c4.xlarge"
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
    s3_input_path,
    content_type = "text/csv",
    split_type = "Line",
    wait = TRUE,
    logs = FALSE
  )

  # TODO: make s3_path generic so it knows how
  #       to take a s3_path object and not double
  #       transform it like "s3://s3://".
  #       I think there are some examples in Shiny
  #       or htmltools.
  s3_predictions_path <- paste0(
    predict_transformer$output_path, "/",
    paste0(basename(s3_input_path), ".out")
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
