test_that("DynatraceLayout.format_event() - with default settings - format event correctly", {

  # Arrange
  event <- LogEvent$new(
    logger = Logger$new("dum/my"),
    level = 200L,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt"), tzone = "UTC"),
    caller = "foonction",
    msg = "foo bar",
    rawMsg = "foobar-raw"
  )

  # Act
  res <- jsonlite::fromJSON(DynatraceLayout$format_event(event))

  # Assert
  expect_setequal(
    names(res),
    c("content", "loglevel", "log.logger", "log.raw_level", "timestamp", "code.function", "log.record.template"))

  expect_identical(res$loglevel, "error")
  expect_identical(res$log.raw_level, "error")
  expect_identical(res$timestamp, "2018-11-02 16:19:33")
  expect_identical(res$log.logger, "dum.my")
  expect_identical(res$code.function, "foonction")
  expect_identical(res$content, "foo bar")
  expect_identical(res$log.record.template, "foobar-raw")
})
