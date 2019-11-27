
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sagemaker

<!-- badges: start -->

<!-- badges: end -->

The sagemaker R package provides a simplified interface to the AWS
Sagemaker API by:

1.  adding sensible defaults so you can dive in quickly

2.  creating helper functions to streamline model analysis

3.  supporting `data.frame`s and
    [tibbles](https://github.com/tidyverse/tibble)

Check out the [Get
started](https://tmastny.github.io/sagemaker/articles/sagemaker.html)
guide for examples\!

## Installation

You can install sagemaker from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tmastny/sagemaker")
```

You will also need `boto3`, `sagemaker`, and `awscli` python packages.
If you don’t have them, or aren’t sure, install with:

``` r
sagemaker::sagemaker_install()
```

Next, you’ll need an [AWS
account](https://aws.amazon.com/premiumsupport/knowledge-center/create-and-activate-aws-account/).
This is a complicated process and unfortunately I don’t have a good
guide to share. Please reach out if you find one and I’ll post it here\!

Once you have the account and user setup you’ll want to save your
account access keys.
[This](https://www.viget.com/articles/set-up-aws-cli-and-download-your-s3-files-from-the-command-line/)
is a good guide to follow.

Lastly, you need a AWS Sagemaker Execution Role. The easiest way is to
create a AWS Sagemaker Notebook Instance. Try [this
guide](https://sagemaker-workshop.com/introduction/notebook.html#launch-the-notebook-instance)
to get the notebook running. Once the notebook is deployed, *inside the
instance* run:

``` python
from sagemaker import get_execution_role

role = get_execution_role()
print(role)
```

Once you have that role, run this command locally to save it with your
local AWS config:

``` r
sagemaker::sagemaker_save_execution_role(
  "arn:aws:iam::[account_number]:role/service-role/[SageMakerExecutionRole]"
)
```

## Streamlined Analysis

Compare the AWS Sagemaker API vs. to the sagemaker R package yourself
with this [side-by-side
comparsion](https://tmastny.github.io/sagemaker/articles/sagemaker-vs-sagemaker.html)
. The R package hides the details for latter, and lets you get started
ASAP.

You start with this:

``` r
library(sagemaker)
xgb <- sagemaker_xgb_estimator()
```

instead of this:

``` r
library(reticulate)
sagemaker <- reticulate::import("sagemaker")
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
