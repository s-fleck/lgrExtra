test_that("transform_event_dynatrace transforms correctly", {
  event <- LogEvent$new(
    logger = Logger$new("dum/my"),
    level = 200L,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    caller = NA_character_,
    msg = "foo bar",
    rawMsg = "foobar-raw"
  )

  result <- transform_event_dynatrace(event)

  expect_type(result, "list")

  expect_setequal(
    names(result),
    c("content",
      "loglevel",
      "log.logger",
      "log.raw_level",
      "timestamp",
      "code.function",
      "content_raw")
  )

  expect_equal(result$content, "foo bar")
  expect_equal(result$loglevel, "error")
  expect_equal(result[["log.logger"]], "dum.my")
  expect_equal(result[["log.raw_level"]], "error")
  expect_equal(result[["content_raw"]], "foobar-raw")
})
