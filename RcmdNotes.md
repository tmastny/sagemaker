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
─  preparing ‘sagemaker’: (3.4s)
✔  checking DESCRIPTION meta-information ...
─  installing the package to build vignettes
✔  creating vignettes (34.5s)
─  checking for LF line-endings in source and make files and shell scripts
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
* checking top-level files ... NOTE
Non-standard file/directory found at top level:
  ‘todo.md’
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
* checking dependencies in R code ... WARNING
'::' or ':::' import not declared from: ‘readr’
* checking S3 generic/method consistency ... WARNING
sagemaker_download_model:
  function(x, path)
    sagemaker_download_model.character:
  function(training_job_name, path)

    sagemaker_download_model:
  function(x, path)
    sagemaker_download_model.sagemaker:
  function(object, path)

    sagemaker_load_model:
  function(x)
    sagemaker_load_model.character:
  function(training_job_name)

    sagemaker_load_model:
  function(x)
    sagemaker_load_model.sagemaker:
  function(object)

    sagemaker_tuning_job_logs:
  function(x)
    sagemaker_tuning_job_logs.character:
  function(tuning_job_name)

    sagemaker_tuning_job_logs:
  function(x)
    sagemaker_tuning_job_logs.sagemaker:
  function(object)

    predict:
  function(object, ...)
    predict.sagemaker:
  function(object, new_data)

    See section ‘Generic functions and methods’ in the ‘Writing R
Extensions’ manual.
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... NOTE
format_endpoint_predictions: no visible binding for global variable ‘.’
format_endpoint_predictions: no visible global function definition for
‘tibble’
format_endpoint_predictions: no visible binding for global variable
‘.pred’
print.sagemaker: no visible global function definition for
‘capture.output’
s3: no visible binding for global variable ‘.’
sagemaker_attach_tuner: no visible binding for global variable
‘final_objective_value’
sagemaker_attach_tuner: no visible binding for global variable
‘training_job_name’
sagemaker_training_job_logs: no visible binding for global variable
‘logs’
sagemaker_training_job_logs: no visible binding for global variable
‘iteration’
sagemaker_training_job_logs: no visible global function definition for
‘:=’
sagemaker_training_job_logs: no visible binding for global variable ‘.’
Undefined global functions or variables:
  . .pred := capture.output final_objective_value iteration logs tibble
training_job_name
Consider adding
importFrom("utils", "capture.output")
to your NAMESPACE file.
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... WARNING
Undocumented arguments in documentation object 'read_s3'
‘x’

Undocumented arguments in documentation object 'sagemaker_download_model'
‘path’

Undocumented arguments in documentation object 'sagemaker_ranges'
‘type’

Functions with \usage entries need to have the appropriate \alias
entries, and all their arguments documented.
The \usage entries must correspond to syntactically valid R code.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... WARNING
Warning: package needs dependence on R (>= 2.10)
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ...
Running ‘testthat.R’ [0m/16m]
OK
* checking for unstated dependencies in vignettes ... NOTE
'::' or ':::' import not declared from: ‘rsample’
'library' or 'require' calls not declared from:
  ‘ggplot2’ ‘rsample’
* checking package vignettes in ‘inst/doc’ ... OK
* checking re-building of vignette outputs ... OK
* DONE

Status: 4 WARNINGs, 3 NOTEs
See
‘/Users/Tim/rpackages/sagemaker.Rcheck/00check.log’
for details.


── R CMD check results ──────────────────────────────────── sagemaker 0.0.1 ────
Duration: 17m 28.1s

❯ checking dependencies in R code ... WARNING
'::' or ':::' import not declared from: ‘readr’

❯ checking S3 generic/method consistency ... WARNING
sagemaker_download_model:
  function(x, path)
    sagemaker_download_model.character:
  function(training_job_name, path)

    sagemaker_download_model:
  function(x, path)
    sagemaker_download_model.sagemaker:
  function(object, path)

    sagemaker_load_model:
  function(x)
    sagemaker_load_model.character:
  function(training_job_name)

    sagemaker_load_model:
  function(x)
    sagemaker_load_model.sagemaker:
  function(object)

    sagemaker_tuning_job_logs:
  function(x)
    sagemaker_tuning_job_logs.character:
  function(tuning_job_name)

    sagemaker_tuning_job_logs:
  function(x)
    sagemaker_tuning_job_logs.sagemaker:
  function(object)

    predict:
  function(object, ...)
    predict.sagemaker:
  function(object, new_data)

    See section ‘Generic functions and methods’ in the ‘Writing R
Extensions’ manual.

❯ checking Rd \usage sections ... WARNING
Undocumented arguments in documentation object 'read_s3'
‘x’

Undocumented arguments in documentation object 'sagemaker_download_model'
‘path’

Undocumented arguments in documentation object 'sagemaker_ranges'
‘type’

Functions with \usage entries need to have the appropriate \alias
entries, and all their arguments documented.
The \usage entries must correspond to syntactically valid R code.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.

❯ checking data for ASCII and uncompressed saves ... WARNING
Warning: package needs dependence on R (>= 2.10)

❯ checking top-level files ... NOTE
Non-standard file/directory found at top level:
  ‘todo.md’

❯ checking R code for possible problems ... NOTE
format_endpoint_predictions: no visible binding for global variable ‘.’
format_endpoint_predictions: no visible global function definition for
‘tibble’
format_endpoint_predictions: no visible binding for global variable
‘.pred’
print.sagemaker: no visible global function definition for
‘capture.output’
s3: no visible binding for global variable ‘.’
sagemaker_attach_tuner: no visible binding for global variable
‘final_objective_value’
sagemaker_attach_tuner: no visible binding for global variable
‘training_job_name’
sagemaker_training_job_logs: no visible binding for global variable
‘logs’
sagemaker_training_job_logs: no visible binding for global variable
‘iteration’
sagemaker_training_job_logs: no visible global function definition for
‘:=’
sagemaker_training_job_logs: no visible binding for global variable ‘.’
Undefined global functions or variables:
  . .pred := capture.output final_objective_value iteration logs tibble
training_job_name
Consider adding
importFrom("utils", "capture.output")
to your NAMESPACE file.

❯ checking for unstated dependencies in vignettes ... NOTE
'::' or ':::' import not declared from: ‘rsample’
'library' or 'require' calls not declared from:
  ‘ggplot2’ ‘rsample’

0 errors ✔ | 4 warnings ✖ | 3 notes ✖
Error: R CMD check found WARNINGs
Execution halted

Exited with status 1.
