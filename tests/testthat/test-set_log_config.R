test_that("get config fetches defaults", {
  expect_identical(get_log_config("log_fields"), c("level", "time", "msg"))
  expect_null(get_log_config("additional_fields"), NULL)
  expect_true(get_log_config("enforce_ascii_msg"), TRUE)
  expect_true(get_log_config("enforce_tz_utc"), TRUE)
  expect_true(get_log_config("log_to_azure"), TRUE)
  expect_match(get_log_config("log_type"), "log_from_r")
  expect_match(get_log_config("customer_id_env"), "AZ_LOG_ID")
  expect_match(get_log_config("shared_key_env"), "AZ_LOG_KEY")
})
test_that("set & get config works", {
  expect_true(get_log_config("log_to_azure"), TRUE)

  set_log_config(enforce_tz_utc = FALSE, log_to_azure = FALSE)
  expect_false(get_log_config("enforce_tz_utc"), FALSE)
  expect_false(get_log_config("log_to_azure"), FALSE)

  set_log_config()
  expect_true(get_log_config("log_to_azure"), TRUE)
})
test_that("get config key can be null", {
  set_log_config(enforce_tz_utc = FALSE, log_to_azure = FALSE)
  expect_identical(
    get_log_config(),
    list(
      log_fields = c("level", "time", "msg"),
      additional_fields = NULL,
      enforce_ascii_msg = TRUE,
      enforce_tz_utc = FALSE,
      log_to_azure = FALSE,
      log_type = "log_from_r",
      customer_id_env = "AZ_LOG_ID",
      shared_key_env = "AZ_LOG_KEY"
    )
  )
  set_log_config()
  expect_identical(
    get_log_config(),
    list(
      log_fields = c("level", "time", "msg"),
      additional_fields = NULL,
      enforce_ascii_msg = TRUE,
      enforce_tz_utc = TRUE,
      log_to_azure = TRUE,
      log_type = "log_from_r",
      customer_id_env = "AZ_LOG_ID",
      shared_key_env = "AZ_LOG_KEY"
    )
  )
})
