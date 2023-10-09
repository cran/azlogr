
<!-- README.md is generated from README.Rmd. Please edit that file -->

# azlogr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/azlogr)](https://CRAN.R-project.org/package=azlogr)
[![Codecov test
coverage](https://codecov.io/gh/atalv/azlogr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/atalv/azlogr?branch=main)
[![R-CMD-check](https://github.com/atalv/azlogr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/atalv/azlogr/actions/workflows/R-CMD-check.yaml)
[![](https://cranlogs.r-pkg.org/badges/azlogr?color=lightgrey)](https://cran.r-project.org/package=azlogr)
<!-- badges: end -->

The goal of `azlogr` is to enable logging in ‘R’ and easily send the
logging messages to ‘Azure Log Analytics’ workspace in real-time. It
also shows the logging message on ‘R’ console, which makes it easier to
see the logs in the same pace where ‘R’ codes are executed. It will be
easier for somebody to retrieve the historical logs in ‘Azure Log
Analytics’, if needed, and compare.

This is an extension of the
[`'logger'`](https://daroczig.github.io/logger/) package, see this
article: `vignette("Intro", package = "logger")` for an introduction
about that package. You may set the logging threshold using
`logger::log_threshold()` function from `'logger'` package while
initiating a session.

Moreover, there is an option provided to add additional custom meta-data
while logging, which can be helpful at times.

## Installation

You can install the latest version of `azlogr` as published on
[CRAN](https://cran.r-project.org/package=azlogr/) with:

``` r
install.packages("azlogr")
```

Or, install the latest development version of `azlogr` from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("atalv/azlogr")
```

## Example

Below is shown a simple way to use the logging mechanism. Please refer
the vignette article of this package, `vignette("how-to-use-azlogr")`,
to know more on how to configure ‘Azure Log Analytics’ workspace
credentials and use this package easily.

``` r
library(azlogr)
set_log_config(log_to_azure = FALSE)
logger_info("logging info")
#> {"level":"INFO","time":"2023-01-11 13:15:03","msg":"logging info"}
```

### Workflow

And here is an example workflow of configuring the logging mechanism and
using `logger_*` functions to log.

``` r
# Azure Log Analytics workspace id and shared key are fetched
# from environment variables.
# `Sys.setenv` is used only for demonstration purpose!!
Sys.setenv(AZ_LOG_ID = "<enter-your-Azure-Log-Analytics-workspace-id>")
Sys.setenv(AZ_LOG_KEY = "<enter-your-Azure-Log-Analytics-shared-key>")

library(azlogr)

# Optionally, add additional meta-data, `country` and `id`, to be collected
# while logging, on top of the default fields - 'level', 'time', 'msg'.
set_log_config(
  log_fields = c("level", "time", "msg"),
  additional_fields = list(country = "in", id = 123)
)

# Use logger_* functions with appropriate logging level to log.
# If POST is successful, then it will be available in custom log table on
# Azure Log Analytics, by default table name will be `log_from_r`_CL (_CL is
# added by Azure for any custom log table)
logger_info("log info sent to Azure")
#> {"level":"INFO","time":"2023-01-11 13:15:04","msg":"log info sent to Azure","country":"in","id":123}

# If the POST request is unsuccessful due to Azure credential issue, then log
# message is displayed on console and a warning is generated with error details.
logger_info("log info sent to Azure")
#> {"level":"INFO","time":"2023-01-11 13:15:04","msg":"log info sent to Azure","country":"in","id":123}
#> Warning message:
#> In logger_level(logger::INFO, ...) :
#>   Some error happened while sending POST request to 'Azure Log Analytics' workspace.
#> Error message: Error in curl::curl_fetch_memory(url, handle = handle) :
#>   Could not resolve host: abcd.ods.opinsights.azure.com
```
