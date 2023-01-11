#' Setting environment for configuration parameters
#'
#' New environment being created within the package enclosure for ease of
#' reading parameter inputs by other functions. Parameter inputs provided
#' are being directly stored in this environment - which can be referred
#' in other functions.
#' @noRd
config_env <- new.env()


#' Set logging configuration
#'
#' Set the logging configuration once by executing this function. There won't be
#' any need to set them every time while logging something via
#' \code{\link{logger_level}} or any wrapper of that, e.g.
#' \code{\link{logger_info}} function(s).
#'
#' @param log_fields Character vector of field names to be included in the JSON.
#'     These field names are automatically collected by
#'     \code{\link[logger]{get_logger_meta_variables}} function, please refer
#'     to that function's documentation to see which fields are collected.
#' @param additional_fields A named vector of type list with key-value pairs of
#'     additional meta data which needs to be added in logging context on top of
#'     \code{log_fields}. The respective value of each key is expected to be of
#'     length 1. It is \code{NULL} by default.
#' @param enforce_ascii_msg If \code{TRUE} (default), the logging message is
#'     guaranteed to have all non-ASCII characters escaped. If \code{FALSE}, the
#'     characters will be logged as-is. Please note, it is better to ensure
#'     ASCII, otherwise there might be error while sending the HTTP POST request
#'     to 'Azure Log Analytics' workspace.
#' @param enforce_tz_utc If \code{TRUE} (default), the logging time field is
#'     converted to UTC timezone while sending the logging dump to 'Azure Log
#'     Analytics' workspace. If \code{FALSE}, then the local time captured by
#'     \code{\link[base]{Sys.time}} is recorded in the time field.
#' @param log_to_azure If \code{TRUE} (default), then logs will be sent to
#'     'Azure Log Analytics' workspace and console. Else if \code{FALSE} then
#'     logs will not be sent to 'Azure Log Analytics' workspace, it will only be
#'     displayed on console, which is the default layout of \code{'logger'}
#'     package.
#' @param log_type Single element character vector is expected. Logs will be
#'     posted to this event on 'Azure Log Analytics'. For details, check this:
#'     \url{https://learn.microsoft.com/en-us/azure/azure-monitor/logs/data-collector-api?tabs=python/}
#'     . Default value is \code{"log_from_r"}.
#' @param customer_id_env The name of the environment variable (default is
#'     \code{AZ_LOG_ID}) which stores the workspace ID of the 'Azure Log
#'     Analytics' workspace. Please refer
#' \url{https://learn.microsoft.com/en-us/azure/azure-monitor/logs/data-collector-api?tabs=powershell#sample-requests/}
#'     to see how you may get the required workspace ID.
#' @param shared_key_env The name of the environment variable (default is
#'     \code{AZ_LOG_KEY}) which stores the shared key of the 'Azure Log
#'     Analytics' workspace. Please refer
#' \url{https://learn.microsoft.com/en-us/azure/azure-monitor/logs/data-collector-api?tabs=powershell#sample-requests/}
#'     to see how you may get the required shared key.
#'
#' @return It saves the configuration in an environment enclosed within this
#'     package. Returns nothing explicitly.
#' @export
#' @family Logging configurations
#' @examples
#'   set_log_config(log_fields = c("level", "time", "msg", "user", "pid"))
#'   set_log_config(enforce_tz_utc = FALSE, log_to_azure = FALSE)
#'
set_log_config <- function(log_fields = c("level", "time", "msg"),
                           additional_fields = NULL,
                           enforce_ascii_msg = TRUE,
                           enforce_tz_utc = TRUE,
                           log_to_azure = TRUE,
                           log_type = "log_from_r",
                           customer_id_env = "AZ_LOG_ID",
                           shared_key_env = "AZ_LOG_KEY") {
  args_list <- as.list(as.list(environment()))
  assign(".input_default", args_list, envir = config_env)
  invisible()
}

#' Get configuration value
#'
#' Get the configuration value of a specific key which was set (or not set) using
#' \code{\link{set_log_config}} function. If nothing was set, then it reuses the
#' default value defined in the function signature of
#' \code{\link{set_log_config}} function.
#'
#' @param key Specify the key whose value needs to be extracted. \code{NULL}
#'     implies no specific key, rather all of them to be extracted at once.
#'
#' @return Returns the respective configuration value of the given \code{key}.
#'     If \code{key} is \code{NULL}, then all the configuration values will be
#'     returned together as a list.
#' @export
#' @aliases get_log_config
#' @examples
#'   # Get configuration value without setting anything
#'   get_log_config("log_to_azure")
#'   # Set some configuration first and then get the respective values
#'   set_log_config(enforce_tz_utc = FALSE, log_to_azure = FALSE)
#'   get_log_config("enforce_tz_utc")
#'   get_log_config("log_to_azure")
#'   # Reset the values
#'   set_log_config()
#'   get_log_config("log_to_azure")
#'
#'   # Extract list of all configurations
#'   get_log_config()
#'
get_log_config <- function(key = NULL) {
  if (!(".input_default" %in% ls(envir = config_env, all.names = TRUE))) {
    inputs <- formals(set_log_config)
  } else {
    inputs <- config_env[[".input_default"]]
  }
  if (is.null(key)) {
    return(lapply(inputs, eval))
  } else {
    return(eval(inputs[[key]]))
  }
}
