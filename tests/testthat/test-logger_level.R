test_that("if incorrect authorization generate warning", {
  mockery::stub(logger_level, ".post_data", list(status_code = 300))
  mockery::stub(logger_level, "httr::content", "testmock")
  expect_warning(logger_level(logger::INFO, "logging message info",
                              log_to_azure = TRUE),
                 "Could not post to 'Azure Log Analytics'")
})
test_that("posting to AZ log analytics works", {
  mockery::stub(logger_level, ".post_data", list(status_code = 200))
  expect_error(logger_level(logger::INFO, "logging message info",
                            log_to_azure = TRUE),
               NA)
})
test_that("catch error as warn if POST to AZ unsuccessful, console print ok", {
  expect_warning(logger_level(logger::INFO, "logging message",
                              log_to_azure = TRUE,
                              log_customer_id = "abcd",
                              log_shared_key = "abcd"),
                 "Some error happened while sending POST request",
                 ignore.case = TRUE)
  expect_match(capture.output(logger_info("logging message info",
                                          log_to_azure = FALSE),
                              type = "message"),
               "logging message info")
})
test_that("logging to console works", {
  expect_match(capture.output(logger_info("logging message info",
                                          log_to_azure = FALSE),
                              type = "message"),
               "logging message info")
  expect_match(capture.output(logger_warn("logging message warn",
                                          log_to_azure = FALSE),
                              type = "message"),
               "logging message warn")
  expect_match(capture.output(logger_error("logging message error",
                                           log_to_azure = FALSE),
                              type = "message"),
               "logging message error")
})
test_that("additional field works", {
  set_log_config(c("level", "msg", "country", "state"),
                 list(country = "in", state = "hr"))
  expect_match(capture.output(logger_info("logging message info",
                                          log_to_azure = FALSE),
                              type = "message"),
               "logging message info.+country.+in.+state.+hr")
  set_log_config()
})
test_that("enforce ascii works", {
  non_ascii_raw <- raw(3)
  non_ascii_raw[1] <- as.raw(0xe2)
  non_ascii_raw[2] <- as.raw(0x99)
  non_ascii_raw[3] <- as.raw(0xaa)

  set_log_config(enforce_ascii = FALSE)
  expect_match(
    capture.output(
      logger_info(paste0("non-ASCII as-is ", rawToChar(non_ascii_raw)),
                  log_to_azure = FALSE),
      type = "message"
    ),
    paste0("non-ASCII as-is ", rawToChar(non_ascii_raw))
  )

  set_log_config(enforce_ascii = TRUE)
  expect_no_error(
    logger_info(paste0("non-ASCII forced to ASCII ",
                       rawToChar(non_ascii_raw)),
                log_to_azure = FALSE)
  )
  skip_if(
    iconv(
      enc2native(rawToChar(non_ascii_raw)),
      from = "UTF-8", to = "ASCII", sub = "Unicode"
    ) != "<U+266A>",
    message = "iconv didn't convert music to <U+266A>, also no error"
  )
  expect_match(
    capture.output(
      logger_info(paste0("non-ASCII forced to ASCII ",
                         rawToChar(non_ascii_raw)),
                  log_to_azure = FALSE),
      type = "message"
    ),
    "non-ASCII forced to ASCII <U\\+266A>"
  )

  set_log_config()
})
test_that("additional meta vars error catching works", {
  set_log_config(c("level", "msg", "country", "state"),
                 c(country = "in"))
  expect_error(logger_info("logging message info",
                           log_to_azure = FALSE),
               "additional_fields argument is not of expected datatype")
  set_log_config(c("level", "msg", "country", "state"),
                 list("in"))
  expect_error(logger_info("logging message info",
                           log_to_azure = FALSE),
               "It should be named list")
  set_log_config(c("level", "msg", "country", "state"),
                 list(level = c("abc")))
  expect_error(logger_info("logging message info",
                           log_to_azure = FALSE),
               "cannot use any restricted names")
  set_log_config(c("level", "msg", "country", "state"),
                 list(country = c("in", "ro")))
  expect_error(logger_info("logging message info",
                           log_to_azure = FALSE),
               "It should not be multivalued")
  set_log_config()
})
test_that("logging level captured properly", {
  expect_match(capture.output(logger_info("logging message",
                                          log_to_azure = FALSE),
                              type = "message"),
               "level.+INFO")
  expect_match(capture.output(logger_warn("logging message",
                                          log_to_azure = FALSE),
                              type = "message"),
               "level.+WARN")
  expect_match(capture.output(logger_error("logging message",
                                           log_to_azure = FALSE),
                              type = "message"),
               "level.+ERROR")
  expect_match(capture.output(logger_success("logging message",
                                             log_to_azure = FALSE),
                              type = "message"),
               "level.+SUCCESS")
  logger::log_threshold(logger::DEBUG)
  expect_match(capture.output(logger_debug("logging message",
                                           log_to_azure = FALSE),
                              type = "message"),
               "level.+DEBUG")
  logger::log_threshold(logger::TRACE)
  expect_match(capture.output(logger_trace("logging message",
                                           log_to_azure = FALSE),
                              type = "message"),
               "level.+TRACE")
  logger::log_threshold(logger::INFO)
  expect_match(capture.output(logger_fatal("logging message",
                                           log_to_azure = FALSE),
                              type = "message"),
               "level.+FATAL")
})
test_that("posting to AZ considers log threshold", {
  logger::log_threshold(logger::INFO)
  mockery::stub(logger_level, ".post_data", list(status_code = 300))
  mockery::stub(logger_level, "httr::content", "testmock")
  expect_warning(logger_level(logger::INFO, "logging message info",
                              log_to_azure = TRUE),
                 "Could not post to 'Azure Log Analytics'")
  logger::log_threshold(logger::WARN)
  expect_warning(logger_level(logger::INFO, "logging message info",
                              log_to_azure = TRUE),
                 NA)
})
test_that("no logging message does not throw error but warning", {
  logger::log_threshold(logger::INFO)
  expect_error(
    suppressWarnings(logger_level(logger::INFO, log_to_azure = FALSE)), NA
  )
})
