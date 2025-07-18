test_that("LayoutDynatrace.format_event() - with default settings - format event correctly", {

  # Arrange
  event <- LogEvent$new(
    logger = Logger$new("dum/my"),
    level = 200L,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    caller = NA_character_,
    msg = "foo bar",
    rawMsg = "foobar-raw"
  )

  lo <- LayoutDynatrace$new(
    transform_event_names = c(
      "msg" = "content",
      "level" = "loglevel",
      "logger" = "log.logger",
      "caller" = "code.function",
      "rawMsg" = "log.record.template"
    ),
    excluded_fields = NULL
  )

  # Act
  res <- jsonlite::fromJSON(lo$format_event(event))

  # Assert
  expect_setequal(
    names(res),
    c("content",
      "loglevel",
      "log.logger",
      "log.raw_level",
      "timestamp",
      "code.function",
      "log.record.template")
  )
})


test_that("LayoutDynatrace.format_event() - with excluded fields - excludes field correctly", {

  # Arrange
  event <- LogEvent$new(
    logger = Logger$new("dum/my"),
    level = 200L,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    caller = NA_character_,
    msg = "foo bar",
    rawMsg = "foobar-raw"
  )

  lo <- LayoutDynatrace$new(
    transform_event_names = c(
      "msg" = "content",
      "level" = "loglevel",
      "logger" = "log.logger",
      "caller" = "code.function",
      "rawMsg" = "log.record.template"
    ),
    excluded_fields = c("log.record.template", "code.function")
  )

  # Act
  res <- jsonlite::fromJSON(lo$format_event(event))

  # Assert
  expect_setequal(
    names(res),
    c("content", "loglevel", "log.logger", "log.raw_level", "timestamp"))

  expect_identical(res$content, "foo bar")
  expect_identical(res$loglevel, "error")
  expect_identical(res$log.logger, "dum.my")
  expect_identical(res$log.raw_level, "error")
})


test_that("LayoutDynatrace.format_event() - with field rename function - renames fields correctly", {

  # Arrange
  event <- LogEvent$new(
    logger = Logger$new("dum/my"),
    level = 200L,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    caller = NA_character_,
    msg = "foo bar",
    rawMsg = "foobar-raw"
  )

  lo <- LayoutDynatrace$new(
    transform_event_names = toupper,
    excluded_fields = c("RAWMSG", "CALLER")
  )

  # Act
  res <- jsonlite::fromJSON(lo$format_event(event))

  # Assert
  expect_setequal(
    names(res),
    c("LOGGER", "LEVEL", "TIMESTAMP", "MSG", "LOG.RAW_LEVEL"))

  expect_identical(res$MSG, "foo bar")
  expect_identical(res$LEVEL, "error")
  expect_identical(res$LOGGER, "dum.my")
  expect_identical(res$LOG.RAW_LEVEL, "error")
})
