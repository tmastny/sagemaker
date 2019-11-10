# Following guide here: https://rstudio.github.io/reticulate/articles/package.html
# global reference to python packages (will be initialized in .onLoad)
boto3 <- NULL
sagemaker <- NULL

.onLoad <- function(libname, pkgname) {
  # using superassignment to update global reference to python packages
  boto3 <<- reticulate::import("boto3", delay_load = TRUE)
  sagemaker <<- reticulate::import("sagemaker", delay_load = TRUE)
}
