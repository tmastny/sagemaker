#' Sagemaker Parameter Ranges
#'
#' Parameter ranges for Sagemaker hyperparameter tuning.
#'
#' @param min Minimum value of range.
#' @param max Maximum value of range.
#' @param scaling \code{"Auto"}, \code{"Linear"}, \code{"Logarithmic"}, or
#' \code{"ReverseLogarithmic"}.
#' @param values List of strings that match categorical parameters
#' for the \link{sagemaker_estimator}.
#'
#' @export
sagemaker_ranges <- function(
  type = c("integer", "continuous", "categorical"),
  min, max, scaling = "Auto", values = NULL
) {
 switch(
   type,
   integer = sagemaker_integer(min, max, scaling),
   continuous = sagemaker_continuous(min, max, scaling),
   categorical = sagemaker_categorical(values)
 )
}

#' @rdname sagemaker_ranges
#' @export
sagemaker_integer <- function(min, max, scaling = "Auto") {
  min <- as.integer(min)
  max <- as.integer(max)

  stopifnot(length(scaling) == 1)
  stopifnot(scaling %in% c("Auto", "Linear", "Logarithmic", "ReverseLogarithmic"))

  sagemaker$tuner$IntegerParameter(min, max, scaling)
}

#' @rdname sagemaker_ranges
#' @export
sagemaker_continuous <- function(min, max, scaling = "Auto") {

  stopifnot(length(scaling) == 1)
  stopifnot(scaling %in% c("Auto", "Linear", "Logarithmic", "ReverseLogarithmic"))

  sagemaker$tuner$ContinuousParameter(min, max, scaling)
}

#' @rdname sagemaker_ranges
#' @export
sagemaker_categorical <- function(values) {
  sagemaker$tuner$CategoricalParameter(values)
}

#' Start a Sagemaker Hyperparamter Tuning Job
#'
#' Interface to \code{sagemaker$tuner$HyperparameterTuner}.
#'
#' @param estimator Sagemaker estimator from \link{sagemaker_estimator}.
#'
#' @param split Train/validation dataset split from \link{s3_split}.
#'
#' @param hyperparameter_ranges A named list of model hyperparameters
#' with \link{sagemaker_ranges} for tuning.
#'
#' @param strategy Tuning strategy: \code{"Random"} or \code{"Bayesian"}.
#'
#' @param max_jobs Number of unique models to train during tuning.
#'
#' @param max_parallel_jobs Number of models to train simultaneously.
#'
#' @inheritParams sagemaker_deploy_endpoint
#' @export
sagemaker_hyperparameter_tuner <- function(
  estimator,
  split,
  hyperparameter_ranges,
  strategy = "Random",
  max_jobs = 10L,
  max_parallel_jobs = 2L,
  ...
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
    ...
  )

  # TODO: if given a rsplit object, need to upload it s3

  stopifnot("train" %in% names(split))
  stopifnot("validation" %in% names(split))

  split_dict <- reticulate::dict(
    train = sagemaker$s3_input(split$train, content_type = "text/csv"),
    validation = sagemaker$s3_input(split$validation, content_type = "text/csv")
  )

  tuner$fit(split_dict, logs = FALSE)
  tuner$wait()

  sagemaker_attach_tuner(tuner$latest_tuning_job$job_name)
}

#' Attach an Existing Sagemaker Tuning Job
#'
#' @description
#' Attaches and loads an existing Sagemaker Tuning Job.
#' This is useful to analyze a tuning job that was completed in a previous
#' session.
#'
#' Returns the same object as \link{sagemaker_hyperparameter_tuner}.
#'
#' @param tuning_job_name Name of the tuning job, typically something
#' like \code{"xgboost-191114-2052"}.
#' @export
sagemaker_attach_tuner <- function(tuning_job_name) {

  tuner_df <- sagemaker_tuning_job_logs(tuning_job_name)

  model_name <- tuner_df %>%
    dplyr::filter(final_objective_value == min(final_objective_value)) %>%
    dplyr::pull(training_job_name)

  tuner <- sagemaker$tuner$HyperparameterTuner$attach(tuning_job_name)

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
