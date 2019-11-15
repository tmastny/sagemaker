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
  # TODO: this function writes the sagemaker exeuction
  #       role to ~/.aws/config for future use.
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

# use sagemaker.get_execution_role() if on sagemaker notebook instance
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


# is class for generics
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

# resamples resamples rsplit object, or a python dictionary with
#           train/test keys pointing to sagemaker$s3_input paths

# TODO:
# also make it generic based on job name:
# if job name, attach. Otherwise fit.
# if pass "sagemaker.estimator.Estimator" class,
# will need to build. Otherwise, if character attach
# on jobname.
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
  tuner_stats <- sagemaker$HyperparameterTuningJobAnalytics(tuning_job_name)

  tuner_df <- tuner_stats$dataframe() %>%
    janitor::clean_names() %>%
    tibble::as_tibble()

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

  # TODO: cv method should be here
  tuner <- sagemaker$tuner$HyperparameterTuner$attach(tuning_job_name)

  model_obj <- list(
    model_name = model_name,
    eval_metric = tuner$estimator$hyperparam_dict$eval_metric,
    strategy = tuner$strategy,
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
    "Name:", x$model_name, "\n",
    "Tuning Strategy:", x$strategy, "\n",
    "Evaluation Metric:", x$eval_metric, "\n\n",
    "Best hyperparameters:\n\n"
  )
  print(x$best_tune)
  invisible(x)
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

#' @export
predict.sagemaker <- function(
  object,
  new_data,
  output_path,
  return_data = TRUE,
  instance_count = 1L,
  instance_type = "ml.c4.xlarge"
) {

  predict_estimator <- sagemaker$estimator$Estimator$attach(
    training_job_name = object$model_name
  )

  predict_transformer <- predict_estimator$transformer(
    instance_count = instance_count,
    instance_type = instance_type,
    output_path = output_path,
    assemble_with = 'Line'
  )

  predict_transformer$transform(
    new_data,
    content_type = "text/csv",
    split_type = "Line",
    wait = TRUE,
    logs = FALSE
  )

  s3_predictions_path <- file.path(
    predict_transformer$output_path,
    paste0(basename(new_data), ".out")
  )

  if (!return_data) {
    return(s3_predictions_path)
  }

  temp <- tempfile()

  download_file(s3_predictions_path, temp)
  readr::read_csv(temp)
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
