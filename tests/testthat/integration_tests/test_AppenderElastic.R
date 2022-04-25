# requires a local elastic search instance


test_that("AppenderElastic round trip", {
  con <- elastic::connect("127.0.0.1")
  on.exit(elastic::index_delete(con, index))

  index <- paste(sample(letters, 48, replace = TRUE), collapse = "")

  app <- AppenderElastic$new(con, index)

  lg <-
    get_logger("test/AppenderElastic")$
    add_appender(app)$
    set_propagate(FALSE)

  suppressWarnings({
    lg$info("test 1", baz = "hash")
    lg$error("test 2", error = "404")
    lg$fatal("test 3", foo = "asf")
    lg$fatal("test 4", foo = 1.2)
  })

  user <- list(c(user = "max mustermann"), underlyings = c("tesla", "amazon"))
  lg$fatal("test 5", foo = user)

  app$flush()

  log_data <- app$data

  identical(log_data$msg, paste("test", 1:5))
  identical(log_data$baz, c("hash", rep(NA, 4)))
  identical(log_data$error, c(NA, "404", rep(NA, 3)))
  identical(log_data$foo, list("asf", 1.2, user))
})
