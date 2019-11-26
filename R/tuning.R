#' @export
sagemaker_integer <- function(min, max, scaling = "Auto") {
  min <- as.integer(min)
  max <- as.integer(max)

  stopifnot(length(scaling) == 1)
  stopifnot(scaling %in% c("Auto", "Linear", "Logarithmic", "ReverseLogarithmic"))

  sagemaker$tuner$IntegerParameter(min, max, scaling)
}

#' @export
sagemaker_continuous <- function(min, max, scaling = "Auto") {

  stopifnot(length(scaling) == 1)
  stopifnot(scaling %in% c("Auto", "Linear", "Logarithmic", "ReverseLogarithmic"))

  sagemaker$tuner$ContinuousParameter(min, max, scaling)
}

#' @export
sagemaker_categorical <- function(values) {
  sagemaker$tuner$CategoricalParameter(values)
}
