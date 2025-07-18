test_that("ElasticsearchLayout.format_event() - with default settings - format event correctly", {

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
    c("level", "@timestamp", "logger", "callSite", "message", "rawMessage"))

  expect_identical(res$level, "Error")
  expect_identical(res[["@timestamp"]], 1541175573930.8)
  expect_identical(res$logger, "dum.my")
  expect_identical(res$callSite, "foonction")
  expect_identical(res$message, "foo bar")
  expect_identical(res$rawMessage, "foobar-raw")
})
