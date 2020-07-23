context("AppenderSendmail")

email <- whoami::email_address()
smtp  <- getOption("stat.smtp_server")

test_that("AppenderSendmail: to_syslog_level works", {

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
