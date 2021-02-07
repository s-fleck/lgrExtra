email <- whoami::email_address()
smtp  <- getOption("stat.smtp_server")

test_that("AppenderSendmail works", {
  lg <- lgr::get_logger("lgrExtra/test/mail")$
    set_propagate(FALSE)$
    add_appender(AppenderSendmail$new(
      from = email,
      to = email,
      control = list(smtpServer = smtp)
    ))

  lg$info("test")
  lg$fatal("FATAL should trigger an email by default")
})




test_that("AppenderGmail: to_syslog_level works", {

  lg <- lgr::get_logger("lgrExtra/test/mail")$
    set_propagate(FALSE)$
    add_appender(AppenderGmail$new(
      from = email,
      to = email
    ))

  lg$info("test")
  lg$fatal("FATAL should trigger an email by default")
})




test_that("AppenderPushbullet: to_syslog_level works", {
  # https://www.pushbullet.com/#settings
  app <- AppenderPushbullet$new(apikey = getOption("rpushbullet.key"))

  lg <- lgr::get_logger("test/pb")$
    add_appender(app, "pb")$
    set_propagate(FALSE)

  lg$fatal("info")
  lg$fatal("test")

  invisible(lg$config(NULL))
})
