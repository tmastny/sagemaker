# sagemaker 0.0.1

* Introducing `sagemaker_load_model` to download and load
  xgboost models to your R session. 
  This requires the Python xgboost package, 
  which you can install with `sagemaker_install_xgboost`
  (the xgboost R package is not sufficient, 
  because Sagemaker pickles the model object).
* Support for multiclass probability parsing on the Sagemaker endpoint.
* Documentation on xgboost multi-class classification.

# sagemaker 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Initial release!
* Blog post: https://timmastny.rbind.io/blog/aws-sagemaker-r/
