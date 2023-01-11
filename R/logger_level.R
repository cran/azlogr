#' Build API signature for logging to 'Azure Log Analytics'
#'
#' 'Azure Log Analytics' HTTP REST API documentation for 'Python' is followed to
#' create the 'R' version of it. 'Python' version of this function is described
#' at \url{https://learn.microsoft.com/en-us/azure/azure-monitor/logs/data-collector-api?tabs=python#sample-requests/}
#'
#' @param customer_id \code{customer_id} of the 'Azure Log Analytics' workspace
#' @param shared_key \code{shared_key} of the 'Azure Log Analytics' workspace
#' @param date date-time of logging event
#' @param content_length Content length of the body
#' @param method Only one value is expected - \code{POST}
#' @param content_type Only one value is expected - \code{application/json}
#' @param resource Only one value is expected - \code{/api/logs}
#' @return Returns part of the header of HTTP POST request to be sent to 'Azure
#'     Log Analytics' workspace
.build_signature <- function(customer_id, shared_key, date, content_length,
                             method, content_type, resource) {
  x_headers <- paste0("x-ms-date:", date)
  string_to_hash <- paste0(method, "\n", content_length, "\n",
                           content_type, "\n", x_headers, "\n", resource)
  bytes_to_hash <- charToRaw(enc2utf8(string_to_hash))
  decoded_key <- caTools::base64decode(shared_key, "raw")
  encoded_hash <- caTools::base64encode(digest::hmac(decoded_key, bytes_to_hash,
                                                     algo = "sha256",
                                                     serialize = FALSE,
                                                     raw = TRUE))
  authorization <- paste0("SharedKey ", customer_id, ":", encoded_hash)
  return(authorization)
}


#' Build and send a request to the POST API of 'Azure Log Analytics'
#'
#' @inherit .build_signature details
#'
#' @param customer_id \code{customer_id} of the 'Azure Log Analytics' workspace
#' @param shared_key \code{shared_key} of the 'Azure Log Analytics' workspace
#' @param body Content or message to be logged in JSON format
#' @param log_type Log-Type as defined in 'Azure Log Analytics' document, for
#'     custom logging
#' @return Returns the HTTP response object
.post_data <- function(customer_id, shared_key, body, log_type) {
  method <- "POST"
  content_type <- "application/json"
  resource <- "/api/logs"
  rfc1123date <- as.character(strftime(as.POSIXlt(Sys.time(), tz = "UTC"),
                                       "%a, %d %b %Y %H:%M:%S GMT"))
  content_length <- nchar(body)
  signature <- .build_signature(customer_id, shared_key, rfc1123date,
                                content_length, method, content_type, resource)
  uri <- paste0("https://", customer_id, ".ods.opinsights.azure.com",
                resource, "?api-version=2016-04-01")
  headers <- httr::add_headers(.headers = c("Content-Type" = content_type,
                                            "Authorization" = signature,
                                            "Log-Type" = log_type,
                                            "x-ms-date" = rfc1123date))
  response <- httr::POST(uri, config = headers, body = body)
  return(response)
}


#' Add additional meta variable
#'
#' Add additional meta variables in the logging context on top of the ones
#' that are readily collected in \code{\link[logger]{get_logger_meta_variables}}
#' function. It might be needed to add some other metadata specific to the
#' logging usage - that goal is served by this function.
#'
#' @inheritParams set_log_config
#' @inheritParams logger::get_logger_meta_variables
#'
#' @return Returns a vector of collected meta-data. It is used in defining the
#'     \code{\link[logger]{log_layout}} function.
#'
.add_meta_variables <- function(additional_fields = NULL,
                               log_level = NULL,
                               namespace = NA_character_,
                               .logcall = sys.call(),
                               .topcall = sys.call(-1),
                               .topenv = parent.frame()) {
  original_meta <- logger::get_logger_meta_variables(
    log_level = log_level,
    namespace = namespace,
    .logcall = .logcall,
    .topcall = .topcall,
    .topenv = .topenv
  )
  if (!is.null(additional_fields) &&
      !is.vector(additional_fields, mode = "list")) {
    error_msg <- paste0("additional_fields argument is not of expected ",
                        "datatype. It should be a vector of type list.")
    stop(error_msg)
  }
  check_error_cond <- (any(
    length(names(additional_fields)) == 0, names(additional_fields) == ""
  ) &&
    (length(additional_fields) > 0))
  if (check_error_cond) {
    error_msg <- paste0("User input in additional_fields argument is not in ",
                        "expected structure. It should be named list of ",
                        "key-value pairs like list(a='abc', b=123). ",
                        "Call to function provided as: ",
                        deparse(match.call()))
    stop(error_msg)
  }
  if (any(names(additional_fields) %in% c(names(original_meta), "msg"))) {
    error_msg <- paste0(
      "Input additional_fields argument cannot use any restricted names as: ",
      paste(c(names(original_meta), "msg"), collapse = ", "), ". Found ",
      "violations with these keys: ",
      names(additional_fields)[
        names(additional_fields) %in% c(names(original_meta), "msg")
      ]
    )
    stop(error_msg)
  }
  if (any(vapply(additional_fields, length, 1) != 1)) {
    error_msg <- paste0(
      "Input additional_fields argument had at least one element of length ",
      "not 1. It should not be multivalued. Violation found with these keys: ",
      names(additional_fields)[vapply(additional_fields, length, 1) != 1]
    )
    stop(error_msg)
  }
  return(c(original_meta, additional_fields))
}


#' Customized logging layout
#'
#' This is an extended function of \code{\link[logger]{layout_json}} function
#' from \code{'logger'} package. Objective is to add additional component in the
#' logging layout in JSON format so that they can also be reported while
#' logging along with the components collected by
#' \code{\link{.add_meta_variables}}.
#'
#' @param log_fields Vector of components which are collected in
#'     \code{\link[logger]{get_logger_meta_variables}} function. Converting
#'     \code{time} component to UTC additionally.
#' @inheritParams set_log_config
#'
#' @return Returns a generator function typically to be used by
#'     \code{\link[logger]{log_layout}} function.
#'
.layout_json_custom <- function(log_fields = c("time", "level", "ns", "ans",
                                               "topenv", "fn", "node", "arch",
                                               "os_name", "os_release",
                                               "os_version", "pid", "user",
                                               "msg"),
                                additional_fields = NULL,
                                enforce_ascii_msg = TRUE,
                                enforce_tz_utc = TRUE) {

  force(log_fields)
  force(additional_fields)
  force(enforce_ascii_msg)
  force(enforce_tz_utc)

  structure(function(level, msg,
                     namespace = NA_character_,
                     .logcall = sys.call(), .topcall = sys.call(-1),
                     .topenv = parent.frame()) {

    json <- .add_meta_variables(additional_fields,
                               log_level = level, namespace = namespace,
                               .logcall = .logcall, .topcall = .topcall,
                               .topenv = .topenv)
    if (enforce_tz_utc) {
      json[["time"]] <- as.POSIXlt(json[["time"]], tz = "UTC")
    }
    sapply(msg, function(msg) {
      if (enforce_ascii_msg) {
        msg <- iconv(msg, to = "ASCII", sub = "Unicode")
      }
      jsonlite::toJSON(
        c(json, list(msg = msg))[unique(c(log_fields,
                                          names(additional_fields)))],
        auto_unbox = TRUE
      )
    })

  }, generator = deparse(match.call()))

}


#' Logging related functions
#'
#' Logger function defined which are created on top of
#' \code{\link[logger]{log_level}} and \code{\link[logger]{layout_json}} -
#' these are part of another package \code{'logger'}. Additional
#' capabilities have been added to those functions which enables this function
#' to be able to send logs directly to the 'Azure Log Analytics' workspace, and
#' also have control to post log outputs into the console - as per user input.
#' Note that, logging threshold can be directly set (if needed) using
#' \code{\link[logger]{log_threshold}} function from \code{'logger'} package.
#'
#' @param ... Content(s) of this argument is directly passed on to
#'     \code{\link[logger]{log_level}} function of the \code{'logger'}
#'     package.
#' @inheritParams set_log_config
#' @param log_customer_id Workspace ID of 'Azure Log Analytics' workspace. By
#'     default it fetches from the environment variable named \code{AZ_LOG_ID}.
#'     If the environment variable is not set, then a dummy value \code{"abcd"}
#'     is used. The environment variable's name can be modified by
#'     \code{\link{set_log_config}}
#' @param log_shared_key Shared key of 'Azure Log Analytics' workspace. By
#'     default it fetches from the environment variable named \code{AZ_LOG_KEY}.
#'     If the environment variable is not set, then a dummy value \code{"abcd"}
#'     is used. The environment variable's name can be modified by
#'     \code{\link{set_log_config}}
#'
#' @return If \code{log_to_azure} is \code{FALSE} then log output is shown on
#'     console. Else, if \code{TRUE}, then log output is shown on console, as
#'     well as posted to 'Azure Log Analytics' workspace under the custom table
#'     name as specified by \code{log_type} argument. If POST request is
#'     unsuccessful, then additional warning message is thrown with POST request
#'     response. If POST request is successful, then it invisibly returns the
#'     \code{\link[httr]{POST}} object.
#' @note Logging layout is set in JSON format, required to send to 'Azure Log
#'     Analytics'. Note that this layout modifies the global \code{namespace} of
#'     \code{'logger'} package by default - that is not important for this use
#'     case.
#' @details \itemize{
#'     \item Most of the arguments of this function have a default value which
#'      is read from the output of \code{\link{get_log_config}}. The idea is
#'      to run the \code{\link{set_log_config}} function once to define the
#'      default arguments; and use them automatically while logging anything
#'      without the need of specifying them every time it is triggered.
#'     \item 'Azure Log Analytics' workspace id and shared key are intentionally
#'      fetched from environment variables for security purpose. It is not
#'      a good practice to specify them explicitly. Using environment variable
#'      is one easy approach to potentially hide it from unintentional user.
#'     }
#' @export
#' @family logging functions
#' @importFrom logger layout_simple
#' @importFrom utils capture.output
#' @rdname logging
#' @aliases logger_level logger_info logger_error logger_warn logger_fatal
#'     logger_success logger_debug logger_trace
#' @examples
#' # Define logging config and then use logger_* functions to log
#' set_log_config(log_to_azure = FALSE)
#' logger_level(logger::INFO, "logging message")
#' # Specify other arguments explicitly inside the logger_level function
#' logger_level(logger::INFO, "logging message", log_to_azure = FALSE)
#'
logger_level <- function(
    ...,
    log_fields = get_log_config("log_fields"),
    additional_fields = get_log_config("additional_fields"),
    enforce_ascii_msg = get_log_config("enforce_ascii_msg"),
    enforce_tz_utc = get_log_config("enforce_tz_utc"),
    log_to_azure = get_log_config("log_to_azure"),
    log_type = get_log_config("log_type"),
    log_customer_id = Sys.getenv(get_log_config("customer_id_env"), "abcd"),
    log_shared_key = Sys.getenv(get_log_config("shared_key_env"), "abcd")
    ) {
  logger::log_layout(
    .layout_json_custom(log_fields, additional_fields,
                        enforce_ascii_msg, enforce_tz_utc)
  )
  logger::log_level(...)
  if (log_to_azure) {
    body <- capture.output(logger::log_level(...), type = "message")
    response <- try(.post_data(customer_id = log_customer_id,
                               shared_key = log_shared_key,
                               body, log_type), silent = TRUE)
    if (inherits(response, "try-error")) {
      warning(paste0("Some error happened while sending POST request ",
                     "to 'Azure Log Analytics' workspace. Error message: ",
                     as.character(response)))
    } else if (response$status_code >= 200 && response$status_code <= 299) {
      invisible(response)
    } else {
      warning(paste0("Could not post to 'Azure Log Analytics', status code: ",
                     response$status_code, ".", "\n",
                     "Response received: ",
                     httr::content(response, as = "text")))
    }
  }
}

#'
#' @rdname logging
#' @aliases logger_info
#' @note \code{logger_info} is a wrapper function around
#'     \code{\link{logger_level}} - logging level is set as
#'     \code{\link[logger]{INFO}} by default.
#'
#' @export
#' @examples
#' # For ease, use wrapper functions instead of using `logger_level` function as
#' # below
#' logger_info("logging message info", log_to_azure = FALSE)
#'
#' # Also, instead of writing `log_to_azure = FALSE` every time, set the
#' # configuration in one step using `set_log_config`, and continue to use
#' # wrapper functions as usual.
#' set_log_config(log_to_azure = FALSE)
#' logger_info("logging message info")
#'
logger_info <- function(...) {
  logger_level(logger::INFO, ...)
}

#'
#' @rdname logging
#' @aliases logger_error
#' @note \code{logger_error} is a wrapper function around
#'     \code{\link{logger_level}} - logging level is set as
#'     \code{\link[logger]{ERROR}} by default.
#'
#' @export
#' @examples
#' # Wrapper function for log level 'error'
#' logger_error("logging message error")
#'
logger_error <- function(...) {
  logger_level(logger::ERROR, ...)
}

#'
#' @rdname logging
#' @aliases logger_warn
#' @note \code{logger_warn} is a wrapper function around
#'     \code{\link{logger_level}} - logging level is set as
#'     \code{\link[logger]{WARN}} by default.
#'
#' @export
#' @examples
#' # Wrapper function for log level 'warn'
#' logger_warn("logging message warn")
#'
logger_warn <- function(...) {
  logger_level(logger::WARN, ...)
}

#'
#' @rdname logging
#' @aliases logger_debug
#' @note \code{logger_debug} is a wrapper function around
#'     \code{\link{logger_level}} - logging level is set as
#'     \code{\link[logger]{DEBUG}} by default.
#'
#' @export
#' @examples
#' # Change log threshold to debug
#' logger::log_threshold(logger::DEBUG)
#' # Wrapper function for log level 'debug'
#' logger_debug("logging message debug")
#'
logger_debug <- function(...) {
  logger_level(logger::DEBUG, ...)
}

#'
#' @rdname logging
#' @aliases logger_fatal
#' @note \code{logger_fatal} is a wrapper function around
#'     \code{\link{logger_level}} - logging level is set as
#'     \code{\link[logger]{FATAL}} by default.
#'
#' @export
#' @examples
#' # Wrapper function for log level 'fatal'
#' logger_fatal("logging message fatal")
#'
logger_fatal <- function(...) {
  logger_level(logger::FATAL, ...)
}

#'
#' @rdname logging
#' @aliases logger_success
#' @note \code{logger_success} is a wrapper function around
#'     \code{\link{logger_level}} - logging level is set as
#'     \code{\link[logger]{SUCCESS}} by default.
#'
#' @export
#' @examples
#' # Wrapper function for log level 'success'
#' logger_success("logging message success")
#'
logger_success <- function(...) {
  logger_level(logger::SUCCESS, ...)
}

#'
#' @rdname logging
#' @aliases logger_trace
#' @note \code{logger_trace} is a wrapper function around
#'     \code{\link{logger_level}} - logging level is set as
#'     \code{\link[logger]{TRACE}} by default.
#'
#' @export
#' @examples
#' # Change logging threshold
#' logger::log_threshold(logger::TRACE)
#' # Wrapper function for log level 'trace'
#' logger_trace("logging message trace")
#'
logger_trace <- function(...) {
  logger_level(logger::TRACE, ...)
}
