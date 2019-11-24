
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sagemaker

<!-- badges: start -->

<!-- badges: end -->

The sagemaker R package provides a simplified API to AWS Sagemaker. The
goal is to provide an R interface that uses sensible defaults so you can
quickly train and analyze machine learning models.

## Installation

You can install sagemaker from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tmastny/sagemaker")
```

## Simplify

With you sagemaker R package, you start here:

``` r
library(sagemaker)
xgb <- sagemaker_xgb_estimator()
```

instead of here:

``` r
library(reticulate)
sage <- reticulate::import("sagemaker")
boto3 <- reticulate::import("boto3")

xgb_container <- sagemaker$amazon$amazon_estimator$get_image_uri(
  boto3$Session()$region_name,
  "xgboost",
  repo_version = "latest"
)

xgb <- sagemaker$estimator$Estimator(
  xgb_container,
  sagemaker_get_execution_role(),
  train_instance_count = 1L,
  train_instance_type = "ml.m4.xlarge",
  output_path = s3(s3_bucket(), "/models/"),
  sagemaker_session = sagemaker$Session()
)
```
