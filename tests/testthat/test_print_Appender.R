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
    print(Appender$new())
    print(AppenderConsole$new())
    print(AppenderConsole$new(layout = LayoutGlue$new()))
    print(AppenderDbi$new(RSQLite::dbConnect(RSQLite::SQLite()), "blubb"))
    print(AppenderSendmail$new("test@blah.com", control = list()))
    print(AppenderBuffer$new(appenders = list(
      AppenderConsole$new(),
      blah = AppenderBuffer$new()
    )))
    print(AppenderPushbullet$new(recipients = "blubb"))
    app$rotate(force = TRUE)
    print(app)
    app$prune(0)

    print(AppenderFileRotatingTime$new(
      tf,
      layout = LayoutJson$new(),
      age = "2 years",
      size = 90000)
    )
    print(AppenderFile$new(tf))
  })
})
