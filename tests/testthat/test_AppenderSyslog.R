context("AppenderSyslog")


test_that("AppenderSyslog: to_syslog_level works", {
  skip_if_not_installed("rsyslog")
  app <- AppenderSyslog$new("myapp")

  expect_identical(
    app$.__enclos_env__$private$to_syslog_levels(c("fatal", "info", "error", "debug", "warn", "trace")),
    c("CRITICAL", "INFO", "ERR", "DEBUG", "WARNING", "DEBUG")
  )

  expect_identical(
    app$.__enclos_env__$private$to_syslog_levels(c("fatal", "info", "error", "debug", "warn", "trace")),
    app$.__enclos_env__$private$to_syslog_levels(c(100, 400, 200, 500, 300, 600))
  )
})




test_that("AppenderSyslog: logging to syslog works", {
  skip_if_not_installed("rsyslog")
  if (!file_is_readable("/var/log/syslog"))
    skip("'/var/log/syslog' is not readable")

  msg <- format(Sys.time())

  lg <- lgr::get_logger("rsyslog/test")$set_propagate(FALSE)
  on.exit(lg$config(NULL))
  lg$add_appender(AppenderSyslog$new(), "syslog")
  lg$info("A test message from R package lgr %s", msg)

  log <- suppressMessages(suppressWarnings( system("cat /var/log/syslog | grep rsyslog/test", intern = TRUE)))
  expect_true(any(grepl(msg, log), fixed = TRUE))
})
