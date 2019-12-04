==> devtools::check()

Updating sagemaker documentation
Writing NAMESPACE
Loading sagemaker
Writing NAMESPACE
── Building ────────────────────────────────── sagemaker ──
Setting env vars:
● CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
● CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
● CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
───────────────────────────────────────────────────────────
✔  checking for file ‘/Users/Tim/rpackages/sagemaker/DESCRIPTION’ ...
─  preparing ‘sagemaker’: (4.3s)
✔  checking DESCRIPTION meta-information ...
─  installing the package to build vignettes
✔  creating vignettes (33.3s)
─  checking for LF line-endings in source and make files and shell scripts (420ms)
─  checking for empty or unneeded directories
─  looking to see if a ‘data/datalist’ file should be added
─  building ‘sagemaker_0.0.1.tar.gz’
   
── Checking ────────────────────────────────── sagemaker ──
Setting env vars:
● _R_CHECK_CRAN_INCOMING_REMOTE_: FALSE
● _R_CHECK_CRAN_INCOMING_       : FALSE
● _R_CHECK_FORCE_SUGGESTS_      : FALSE
── R CMD check ─────────────────────────────────────────────────────────────────
* using log directory ‘/Users/Tim/rpackages/sagemaker.Rcheck’
* using R version 3.6.1 (2019-07-05)
* using platform: x86_64-apple-darwin15.6.0 (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --as-cran’
* checking for file ‘sagemaker/DESCRIPTION’ ... OK
* this is package ‘sagemaker’ version ‘0.0.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking serialization versions ... OK
* checking whether package ‘sagemaker’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking for future file timestamps ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... NOTE
sagemaker_attach_tuner: no visible binding for global variable
  ‘training_job_name’
sagemaker_attach_tuner: no visible binding for global variable
  ‘final_objective_value’
Undefined global functions or variables:
  final_objective_value training_job_name
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ...
  Running ‘testthat.R’ [0m/16m]
 ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
    1. jsonlite:::parse_string(txt, bigint_as_char)
  
  WARNING:sagemaker:Couldn't call 'get_role' to get Role ARN from role name tim.mastny2 to get Role path.
  [16:44:37] WARNING: src/objective/regression_obj.cu:152: reg:linear is now deprecated in favor of reg:squarederror.
  ...............................................!══ testthat results  ══════════════════════════════════════
  [ OK: 14 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 6 ]
  1. Error: endpoint predictions work (@test-ac-endpoint-predictions.R#18) 
  2. Error: one-dimensional endpoint formatting works (@test-prediction-formatting.R#5) 
  3. Error: 2-dimensional endpoint formatting works (@test-prediction-formatting.R#14) 
  4. Error: 5-dimensional endpoint formatting works (@test-prediction-formatting.R#27) 
  5. Error: one-dimensional local and endpoint formatting match (@test-prediction-formatting.R#37) 
  6. Error: 2-dimensional local and endpoint formatting match (@test-prediction-formatting.R#48) 
  
  Error: testthat unit tests failed
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking re-building of vignette outputs ... OK
* DONE

Status: 1 ERROR, 1 NOTE
See
  ‘/Users/Tim/rpackages/sagemaker.Rcheck/00check.log’
for details.

── R CMD check results ──────────────────────────────────── sagemaker 0.0.1 ────
Duration: 17m 4.2s

❯ checking tests ...
  See below...

❯ checking R code for possible problems ... NOTE
  sagemaker_attach_tuner: no visible binding for global variable
    ‘training_job_name’
  sagemaker_attach_tuner: no visible binding for global variable
    ‘final_objective_value’
  Undefined global functions or variables:
    final_objective_value training_job_name

── Test failures ───────────────────────────────────────────────── testthat ────

> library(testthat)
> library(sagemaker)
> 
> test_check("sagemaker")
Loading required package: dplyr

Attaching package: 'dplyr'

The following object is masked from 'package:testthat':

    matches

The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union


Attaching package: 'recipes'

The following object is masked from 'package:stats':

    step

WARNING:sagemaker:Using already existing model: xgboost-191114-2052-001-7b33b7a5
.....................................!WARNING:sagemaker:Using already existing model: xgboost-191114-2052-001-7b33b7a5

-------------------------------------------------------------------------------------------------!── 1. Error: endpoint predictions work (@test-ac-endpoint-p
parse error: trailing garbage
                          10.2579030991,8.10296821594,10.027586937,9.2
                     (right here) ------^
Backtrace:
  1. stats::predict(model, sagemaker::abalone[1:100, -1])
  2. sagemaker:::predict.sagemaker(...)
  3. sagemaker:::format_endpoint_predictions(predictions)
  4. base::as.character(.)
 12. stringr::str_c(., "[", .data, "]")
 13. jsonlite::parse_json(.)
 14. jsonlite:::parse_and_simplify(...)
 15. jsonlite:::parseJSON(txt, bigint_as_char)
  1. jsonlite:::parse_string(txt, bigint_as_char)

── 2. Error: one-dimensional endpoint formatting works (@te
parse error: trailing garbage
                                      1,2,3,4[]
                     (right here) ------^
Backtrace:
  1. testthat::expect_equal(expected_tibble, format_endpoint_predictions(return_string))
  4. sagemaker:::format_endpoint_predictions(return_string)
  5. base::as.character(.)
 13. stringr::str_c(., "[", .data, "]")
 14. jsonlite::parse_json(.)
 15. jsonlite:::parse_and_simplify(...)
 16. jsonlite:::parseJSON(txt, bigint_as_char)
  1. jsonlite:::parse_string(txt, bigint_as_char)

── 3. Error: 2-dimensional endpoint formatting works (@test
parse error: trailing garbage
                                  [1,1],[2,2][]
                     (right here) ------^
Backtrace:
  1. testthat::expect_equal(expected_num_class_2, format_endpoint_predictions(return_string_num_class_2))
  4. sagemaker:::format_endpoint_predictions(return_string_num_class_2)
  5. base::as.character(.)
 13. stringr::str_c(., "[", .data, "]")
 14. jsonlite::parse_json(.)
 15. jsonlite:::parse_and_simplify(...)
 16. jsonlite:::parseJSON(txt, bigint_as_char)
  1. jsonlite:::parse_string(txt, bigint_as_char)

── 4. Error: 5-dimensional endpoint formatting works (@test
parse error: trailing garbage
                            [1,1,1,1,1],[2,2,2,2,2][]
                     (right here) ------^
Backtrace:
  1. testthat::expect_equal(expected_num_class_5, format_endpoint_predictions(return_string_num_class_5))
  4. sagemaker:::format_endpoint_predictions(return_string_num_class_5)
  5. base::as.character(.)
 13. stringr::str_c(., "[", .data, "]")
 14. jsonlite::parse_json(.)
 15. jsonlite:::parse_and_simplify(...)
 16. jsonlite:::parseJSON(txt, bigint_as_char)
  1. jsonlite:::parse_string(txt, bigint_as_char)

── 5. Error: one-dimensional local and endpoint formatting 
parse error: trailing garbage
                                      1,2,3,4[]
                     (right here) ------^
Backtrace:
  1. testthat::expect_equal(expected_tibble, format_endpoint_predictions(return_string))
  4. sagemaker:::format_endpoint_predictions(return_string)
  5. base::as.character(.)
 13. stringr::str_c(., "[", .data, "]")
 14. jsonlite::parse_json(.)
 15. jsonlite:::parse_and_simplify(...)
 16. jsonlite:::parseJSON(txt, bigint_as_char)
  1. jsonlite:::parse_string(txt, bigint_as_char)

── 6. Error: 2-dimensional local and endpoint formatting ma
parse error: trailing garbage
                                  [1,1],[2,2][]
                     (right here) ------^
Backtrace:
  1. testthat::expect_equal(expected_tibble, format_endpoint_predictions(return_string))
  4. sagemaker:::format_endpoint_predictions(return_string)
  5. base::as.character(.)
 13. stringr::str_c(., "[", .data, "]")
 14. jsonlite::parse_json(.)
 15. jsonlite:::parse_and_simplify(...)
 16. jsonlite:::parseJSON(txt, bigint_as_char)
  1. jsonlite:::parse_string(txt, bigint_as_char)

WARNING:sagemaker:Couldn't call 'get_role' to get Role ARN from role name tim.mastny2 to get Role path.
[16:44:37] WARNING: src/objective/regression_obj.cu:152: reg:linear is now deprecated in favor of reg:squarederror.
...............................................!══ testthat results  ══════════════════════════════════════
[ OK: 14 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 6 ]
1. Error: endpoint predictions work (@test-ac-endpoint-predictions.R#18) 
2. Error: one-dimensional endpoint formatting works (@test-prediction-formatting.R#5) 
3. Error: 2-dimensional endpoint formatting works (@test-prediction-formatting.R#14) 
4. Error: 5-dimensional endpoint formatting works (@test-prediction-formatting.R#27) 
5. Error: one-dimensional local and endpoint formatting match (@test-prediction-formatting.R#37) 
6. Error: 2-dimensional local and endpoint formatting match (@test-prediction-formatting.R#48) 

Error: testthat unit tests failed
Execution halted

1 error ✖ | 0 warnings ✔ | 1 note ✖
Error: R CMD check found ERRORs
Execution halted

Exited with status 1.
