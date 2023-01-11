## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----environment--------------------------------------------------------------
Sys.setenv(AZ_LOG_ID = "enter-your-Azure-Log-Analytics-workspace-id")
Sys.setenv(AZ_LOG_KEY = "enter-your-Azure-Log-Analytics-shared-key")

## ----metadata-----------------------------------------------------------------
library(azlogr)
# Collect some additional meta-data on top of the default selection level, time, msg.
# These are captured from the auto-collected components by logger::get_logger_meta_variables.
set_log_config(log_fields = c("level", "time", "msg", "user", "pid"))

# Some additional meta-data to be collected.
set_log_config(
  log_fields = c("level", "time", "msg"),
  additional_fields = list(country = "in", id = 123)
)

# Change the ordering in which the log message is displayed on console.
# Newly added meta-data can also be added in the log_fields arguments to change display order.
set_log_config(
  log_fields = c("country", "id", "time", "level", "msg"),
  additional_fields = list(country = "in", id = 123)
)

## ----setup--------------------------------------------------------------------
set_log_config(
  log_fields = c("country", "id", "time", "level", "msg"),
  additional_fields = list(country = "in", id = 123),
  log_type = "custom_table_r",
  customer_id_env = "ENV_WORKSPACE_ID",
  shared_key_env = "ENV_SHARED_KEY"
)

## ----logging------------------------------------------------------------------
# Add new meta-data as `country = "in"` and `id = 123`.
# Defining the fields to be reported should be: `country`, `id`, `time`,
# `level`, and `msg`. Note that, you may change the order of these fields if
# needed.
set_log_config(
  log_fields = c("country", "id", "time", "level", "msg"),
  additional_fields = list(country = "in", id = 123),
  log_to_azure = FALSE
)

# Once the configuration is done, it is easy to just provide the required
# message using appropriate wrapper functions: logger_info, logger_warn,
# logger_error, etc.
logger_info("log information")
logger_warn("log warning")

## ----threshold----------------------------------------------------------------
logger::log_threshold(logger::WARN)
# Info is not logged when threshold is WARN
logger_info("log information")
logger_warn("log warning")

# Change the threshold
logger::log_threshold(logger::INFO)
# Info is logged now when threshold is INFO
logger_info("log information")

