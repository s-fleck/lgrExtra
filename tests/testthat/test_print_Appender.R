context("print_Appender")




test_that("all Appenders print() without failure", {
  if (
    !requireNamespace("sendmailR") ||
    !requireNamespace("RPushbullet") ||
    !requireNamespace("DBI") ||
    !requireNamespace("RSQLite")
  ){
    skip("Required packages not installed")
  }

  tf <- tempfile()
  on.exit(unlink(tf))

  expect_output({
    print(AppenderDbi$new(RSQLite::dbConnect(RSQLite::SQLite()), "blubb"))
    print(AppenderSendmail$new("test@blah.com", control = list()))
    print(AppenderBuffer$new(appenders = list(
      AppenderConsole$new(),
      blah = AppenderBuffer$new()
    )))
    print(AppenderPushbullet$new(recipients = "blubb"))
  })
})
