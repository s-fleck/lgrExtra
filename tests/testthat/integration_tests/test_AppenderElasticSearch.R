# requires a local elastic search instance
try(source(rprojroot::find_testthat_root_file("integration_tests/elastic_test_user.R")))

test_that("AppenderElasticSearch basic logging works", {
  con <- elastic::connect("127.0.0.1", user = Sys.getenv("ELASTIC_USER"), pwd = Sys.getenv("ELASTIC_PASSWORD"))
  index <- paste(sample(letters, 48, replace = TRUE), collapse = "")
  on.exit({
    elastic::index_delete(con, index)
    lg$config(NULL)
  })

  app <- AppenderElasticSearch$new(con, index)

  lg <-
    get_logger("test/AppenderElasticSearch")$
    add_appender(app)$
    set_propagate(FALSE)


  lg$info("test 1", baz = "hash")
  lg$error("test 2", error = "404")
  lg$fatal("test 3", foo = "asf")
  lg$fatal("test 4", foo = 1.2)
  user <- list(c(user = "max mustermann"), underlyings = c("tesla", "amazon"))
  lg$fatal("test 5", user = user)

  app$flush()
  Sys.sleep(1)

  # check no extra colums with NULL values were added during insert
  raw_log_data <- elastic::Search(con, index, body = '{"query": {"match_all": {}} }')$hits$hits
  expect_identical(
    names(raw_log_data[[1]][["_source"]]),
    c("level", "timestamp", "logger", "caller", "msg", "baz")
  )

  # check data was inserted correctly
  log_data <- app$get_data()
  expect_identical(log_data$msg, paste("test", 1:5))
  expect_identical(log_data$baz, c("hash", rep(NA, 4)))
  expect_identical(log_data$error, c(NA, "404", rep(NA, 3)))
  expect_identical(log_data$foo, c(NA, NA, "asf", 1.2, NA))

  # recursive objects are not round-tripped with the same structure intact,
  # (.e.g vectors might turn into lists, etc..) but the data must stay the same
  expect_null(log_data$user[[1]])
  expect_identical(log_data$user[[5]][[1]], unname(user[[1]]))
  expect_identical(log_data$user[[5]][[2]][[1]], user[[2]][[1]])
  expect_identical(log_data$user[[5]][[2]][[2]], user[[2]][[2]])
})



test_that("AppenderElasticSearch/LayoutElastic transform_names works", {
  con <- elastic::connect("127.0.0.1", user = Sys.getenv("ELASTIC_USER"), pwd = Sys.getenv("ELASTIC_PASSWORD"))
  index <- paste(sample(letters, 48, replace = TRUE), collapse = "")
  on.exit({
    elastic::index_delete(con, index)
    lg$config(NULL)
  })

  app <- AppenderElasticSearch$new(
    con,
    index,
    layout = LayoutElastic$new(transform_names = toupper))

  lg <-
    get_logger("test/AppenderElasticSearch")$
    add_appender(app)$
    set_propagate(FALSE)

  lg$info("test 1", baz = "hash")
  app$flush()
  Sys.sleep(1)

  # check if name transformation from layout was applied
  raw_log_data <- elastic::Search(con, index, body = '{"query": {"match_all": {}} }')$hits$hits
  expect_identical(
    names(raw_log_data[[1]][["_source"]]),
    toupper(c("level", "timestamp", "logger", "caller", "msg", "baz"))
  )
})




test_that("AppenderElasticSearch/LayoutElastic transform_names works", {
  con <- elastic::connect("127.0.0.1", user = Sys.getenv("ELASTIC_USER"), pwd = Sys.getenv("ELASTIC_PASSWORD"))
  index <- paste(sample(letters, 48, replace = TRUE), collapse = "")
  on.exit({
    elastic::index_delete(con, index)
    lg$config(NULL)
  })

  app <- AppenderElasticSearch$new(
    con,
    index,
    layout = LayoutElastic$new(transform_names = toupper))

  lg <-
    get_logger("test/AppenderElasticSearch")$
    add_appender(app)$
    set_propagate(FALSE)

  lg$info("test 1", baz = "hash")
  app$flush()
  Sys.sleep(1)

  # check if name transformation from layout was applied
  raw_log_data <- elastic::Search(con, index, body = '{"query": {"match_all": {}} }')$hits$hits
  expect_identical(
    names(raw_log_data[[1]][["_source"]]),
    toupper(c("level", "timestamp", "logger", "caller", "msg", "baz"))
  )
})




test_that("AppenderElasticSearch/LayoutElastic transform_data works", {
  con <- elastic::connect("127.0.0.1", user = Sys.getenv("ELASTIC_USER"), pwd = Sys.getenv("ELASTIC_PASSWORD"))
  index <- paste(sample(letters, 48, replace = TRUE), collapse = "")
  on.exit({
    elastic::index_delete(con, index)
    lg$config(NULL)
  })

  app <- AppenderElasticSearch$new(
    con,
    index,
    layout = LayoutElastic$new(transform_data = function(.){
      res <- data.table::as.data.table(.)
      res[, test := "it works"]
      res
    }))

  lg <-
    get_logger("test/AppenderElasticSearch")$
    add_appender(app)$
    set_propagate(FALSE)

  lg$info("test 1", baz = "hash")
  lg$info("test 1", foo = "bar")
  app$flush()
  Sys.sleep(1)

  # check if name transformation from layout was applied
  app$get_data()

  expect_identical(app$get_data()[["test"]], rep("it works", 2))
})
