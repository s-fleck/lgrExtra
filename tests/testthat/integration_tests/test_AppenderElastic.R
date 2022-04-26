# requires a local elastic search instance


test_that("AppenderElastic basic logging works", {
  con <- elastic::connect("192.168.21.160", user = Sys.getenv("ELASTIC_USER"), pwd = Sys.getenv("ELASTIC_PASSWORD"))
  index <- paste(sample(letters, 48, replace = TRUE), collapse = "")
  on.exit(elastic::index_delete(con, index))



  app <- AppenderElastic$new(con, index)

  lg <-
    get_logger("test/AppenderElastic")$
    add_appender(app)$
    set_propagate(FALSE)

  lg$info("test 1", baz = "hash")
  lg$error("test 2", error = "404")
  lg$fatal("test 3", foo = "asf")
  lg$fatal("test 4", foo = 1.2)


  user <- list(c(user = "max mustermann"), underlyings = c("tesla", "amazon"))
  lg$fatal("test 5", foo = user)

  app$flush()

  # check no extra colums with NULL values were added during insert
  raw_log_data <- elastic::Search(con, index, body = '{"query": {"match_all": {}} }')$hits$hits
  expect_identical(
    names(raw_log_data[[1]][["_source"]]),
    c("level", "timestamp", "logger", "caller", "msg", "baz")
  )

  # check data was inserted correctly
  log_data <- app$get_data()
  identical(log_data$msg, paste("test", 1:5))
  identical(log_data$baz, c("hash", rep(NA, 4)))
  identical(log_data$error, c(NA, "404", rep(NA, 3)))
  identical(log_data$foo, list("asf", 1.2, user))
})
