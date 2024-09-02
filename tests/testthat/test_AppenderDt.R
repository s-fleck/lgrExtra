context("AppenderDt")

options("datatable.showProgress" = FALSE)

x <- LogEvent$new(
  logger = Logger$new("dummy"),
  level = 200L,
  timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
  caller = NA_character_,
  msg = "foo bar"
)




test_that("AppenderDt: appending multiple rows works", {
  app <- AppenderDt$new()
  y <- x$clone()
  y$level <- seq(100L, 300L, 100L)

  expect_silent(app$append(y))

  expect_true(data.table::is.data.table(app$dt))
  expect_identical(app$data$level[1:3], y$level)
  expect_identical(app$data$timestamp[1:3], rep(y$timestamp, 3))
  expect_identical(app$data$msg[1:3], rep(y$msg, 3))
  expect_identical(app$data$caller[1:3], rep(NA_character_, 3))

  y <- x$clone()
  y$level <- 300
  app$append(y)
  expect_identical(app$.__enclos_env__$private$.data$.id[1:4], 1:4)

  expect_match(paste(capture.output(app$show()), collapse = ""), "ERROR.*WARN")
})




test_that("AppenderDt: works with list columns", {
  app <- AppenderDt$new(
    prototype = data.table::data.table(
      .id = NA_integer_,
      level = NA_integer_,
      timestamp = Sys.Date(),
      msg = NA_character_,
      caller = NA_character_,
      list = list(list())
    )
  )

  e <- LogEvent$new(
    level = 100,
    timestamp = Sys.Date(),
    msg = "blubb",
    caller = "blubb()",
    logger = lgr
  )
  app$append(e)
  expect_true(is.null(app$data$list[[1]]))

  e <- LogEvent$new(
    level = 100,
    timestamp = Sys.Date(),
    msg = "blubb",
    caller = "blubb()",
    logger = lgr,
    list = environment()
  )
  app$append(e)
  expect_true(is.environment(app$data$list[[2]]))

  e <- LogEvent$new(
    level = c(100L, 100L),
    timestamp = Sys.Date(),
    msg = "blubb",
    caller = "blubb()",
    logger = lgr,
    list = iris
  )
  app$append(e)
  expect_true(is.data.frame(app$data$list[[3]]))
  expect_true(is.data.frame(app$data$list[[4]]))

  e <- LogEvent$new(
    level = 100L,
    timestamp = Sys.Date(),
    msg = "blubb",
    caller = "blubb()",
    logger = lgr,
    foo = "bar"
  )
  app$append(e)
  expect_false("foo" %in% names(app$data))

  expect_identical(
    sapply(app$data$list, class),
    c("NULL", "environment", "data.frame", "data.frame", "NULL")
  )
})




test_that("AppenderDt: .fields works", {
  app <- AppenderDt$new()

  e <- LogEvent$new(
    level = 100,
    timestamp = Sys.Date(),
    msg = "blubb",
    caller = "blubb()",
    logger = lgr
  )

  app$append(e)
  expect_true(is_empty(app$data$.fields[[1]]))
  expect_true(is.list(app$data$.fields[[1]]))

  e$envir <- environment()
  e$schwupp = "foo"
  app$append(e)
  expect_identical(app$data$.fields[[2]]$schwupp, "foo")
  expect_true(is.environment(app$data$.fields[[2]]$envir))

  # warn if .fields is not a list column
  expect_warning(
    app <- AppenderDt$new(prototype = data.table::data.table(
      .id = NA_integer_,
      .fields = NA_integer_
    ))
  )
})




test_that("AppenderDt: memory cycling works", {
  app1 <- AppenderDt$new(buffer_size = 10)
  replicate(12, app1$append(x))
  expect_equal(app1$data$.id, 3:12)
  r1 <- app1$data

  # bulk insert behaves like sepparate inserts
  app2 <- AppenderDt$new(buffer_size = 10)
  y <- x$clone()
  y$msg <- rep(y$msg, 12)

  app2$append(y)
  expect_equal(app2$data$.id,  3:12)
  expect_equal(app2$data, r1)
})




test_that("AppenderDt: default format for show_log() looks like format.LogEvent()", {
  lg <- get_logger("test")
  on.exit(lg$config(NULL))
  lg$add_appender(AppenderDt$new(), "memory")

  xo <- capture.output(lg$fatal("blubb"))
  xp <- capture.output(lg$appenders$memory$show(n = 1))
  expect_identical(xo, xp)

  xo <- capture.output(
    lg$fatal("blubb", foo = "bar", fizz = "buzz", iris = iris)
  )
  xp <- capture.output(lg$appenders$memory$show(n = 1))

  expect_identical(xo, xp)
  expect_length(capture.output(lg$appenders$memory$show(n = 2)), 2)
})
