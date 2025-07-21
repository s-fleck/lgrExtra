test_that("ElasticSearchLayout.format_event() - with default settings - format event correctly", {

  # Arrange
  event <- LogEvent$new(
    logger = Logger$new("dum/my"),
    level = 200L,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    caller = "foonction",
    msg = "foo bar",
    rawMsg = "foobar-raw"
  )

  # Act
  res <- jsonlite::fromJSON(ElasticSearchLayout$format_event(event))

  # Assert
  expect_setequal(
    names(res),
    c("log.level", "@timestamp", "log.logger", "log.origin.function", "message", "log.record.template"))

  expect_identical(res$log.leve, "error")
  expect_identical(res[["@timestamp"]], 1541175573930.8)
  expect_identical(res$log.logger, "dum.my")
  expect_identical(res$log.origin.function, "foonction")
  expect_identical(res$message, "foo bar")
  expect_identical(res$log.record.template, "foobar-raw")
})
