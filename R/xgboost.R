sagemaker_xgboost <- function(repo_version = "latest") {

  role = sagemaker$get_execution_role()

  xgb_container <- sagemaker$amazon$amazon_estimator$get_image_uri(
    boto3$Session()$region_name,
    "xgboost",
    repo_version = repo_version
  )


}
