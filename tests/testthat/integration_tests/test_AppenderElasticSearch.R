# requires a local elastic search instance
try(source(rprojroot::find_testthat_root_file("integration_tests/elastic_test_user.R")))

elastic_ip <- "127.0.0.1"


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



test_that("AppenderElasticSearch basic logging works with buffer", {
  con <- elastic::connect("127.0.0.1", user = Sys.getenv("ELASTIC_USER"), pwd = Sys.getenv("ELASTIC_PASSWORD"))
  index <- paste(sample(letters, 48, replace = TRUE), collapse = "")
  on.exit({
    elastic::index_delete(con, index)
    lg$config(NULL)
  })

  app <- AppenderElasticSearch$new(con, index)$
    set_buffer_size(100)

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




test_that("AppenderElasticSearch/LayoutElasticSearch transform_event works", {
  con <- elastic::connect("127.0.0.1", user = Sys.getenv("ELASTIC_USER"), pwd = Sys.getenv("ELASTIC_PASSWORD"))
  index <- paste(sample(letters, 48, replace = TRUE), collapse = "")
  on.exit({
    elastic::index_delete(con, index)
    lg$config(NULL)
  })

  app <- AppenderElasticSearch$new(
    con,
    index,
    layout = LayoutElasticSearch$new(transform_event = function(event){
      res <- event$values
      res[["test"]] <- "it works"
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




test_that("AppenderElasticSearch$get_data() works with numeric levels", {
  con <- elastic::connect("127.0.0.1", user = Sys.getenv("ELASTIC_USER"), pwd = Sys.getenv("ELASTIC_PASSWORD"))
  index <- paste(sample(letters, 48, replace = TRUE), collapse = "")
  on.exit({
    elastic::index_delete(con, index)
    lg$config(NULL)
  })

  app <- AppenderElasticSearch$new(
    con,
    index
  )

  lg <-
    get_logger("test/AppenderElasticSearch")$
    add_appender(app)$
    set_propagate(FALSE)$
    set_threshold(NA)

  lg$trace("test 1")
  lg$debug("test 2")
  lg$info("test 3")
  lg$error("test 3")
  app$flush()
  Sys.sleep(1)

  # check if name transformation from layout was applied
  expect_setequal(app$get_data()$level, c(200, 400, 500, 600))
  expect_setequal(app$get_data(threshold = 400)$level, c(400, 200))
  expect_setequal(app$get_data(threshold = "info")$level, c(400, 200))

  expect_output(app$show(threshold = 400), ".*INFO.*ERROR")

})



test_that("AppenderElasticSearch$get_data() works", {
  con <- elastic::connect("127.0.0.1", user = Sys.getenv("ELASTIC_USER"), pwd = Sys.getenv("ELASTIC_PASSWORD"))
  index <- paste(sample(letters, 48, replace = TRUE), collapse = "")
  on.exit({
    elastic::index_delete(con, index)
    lg$  config(NULL)
  })

  app <- AppenderElasticSearch$new(
    con,
    index,
    layout = LayoutElasticSearch$new(transform_event = function(event){
      res <- event$values
      res[["level"]] <- lgr::label_levels(res[["level"]])
      res
    }))

  lg <-
    get_logger("test/AppenderElasticSearch")$
    add_appender(app)$
    set_propagate(FALSE)$
    set_threshold(NA)

  lg$trace("trace 1")
  lg$debug("debug 2")
  lg$info("info 3")
  lg$error("error 3")
  app$flush()
  Sys.sleep(1)

  # check if name transformation from layout was applied
  expect_setequal(app$get_data()$level, c("trace", "debug", "info", "error"))
  expect_setequal(app$get_data(threshold = 400)$level, c("info", "error"))
  expect_setequal(app$get_data(threshold = "info")$level, c("info", "error"))

  expect_output(app$show(threshold = 400), ".*INFO.*ERROR")
})



test_that("AppenderElasticSearch$get_data() return_type works", {
  con <- elastic::connect("127.0.0.1", user = Sys.getenv("ELASTIC_USER"), pwd = Sys.getenv("ELASTIC_PASSWORD"))
  index <- paste(sample(letters, 48, replace = TRUE), collapse = "")
  on.exit({
    elastic::index_delete(con, index)
    lg$config(NULL)
  })

  app <- AppenderElasticSearch$new(
    con,
    index
  )

  lg <-
    get_logger("test/AppenderElasticSearch")$
    add_appender(app)$
    set_propagate(FALSE)$
    set_threshold(NA)

  lg$trace("test 1")
  lg$debug("test 2")
  lg$info("test 3")
  lg$error("test 3")
  app$flush()
  Sys.sleep(1)

  # check if name transformation from layout was applied
  expect_s3_class(app$get_data(result_type = "data.frame"), "data.frame")
  expect_s3_class(app$get_data(result_type = "data.table"), "data.table")
  expect_type(app$get_data(result_type = "list"), "list")
  expect_true("hits" %in% names(app$get_data(result_type = "list")))
  expect_type(app$get_data(result_type = "json"), "character")
})


test_that("AppenderElasticSearch can create indices from functions", {

  # arrange
  con <- elastic::connect(elastic_ip)

  indices <- c("test_logging_r_1", "test_logging_r_2")
  sel_index <- 1
  index <- function() indices[[sel_index]]

  on.exit({
    try(elastic::index_delete(con, indices[[1]]))
    try(elastic::index_delete(con, indices[[2]]))
    lg$config(NULL)
  })

  app <- AppenderElasticSearch$new(con, index)

  lg <-
    get_logger("test/AppenderElasticSearch")$
    add_appender(app)$
    set_propagate(FALSE)$
    set_threshold(NA)

  # act
  expect_identical(app$index, indices[[1]])
  msg1 <- "this should end up in index-1"
  lg$fatal(msg1, error = "404")
  app$flush()

  sel_index <- 2
  expect_identical(app$index, indices[[2]])
  msg2 <- "this should end up in index-2"
  lg$fatal(msg2, foo = "asf")
  app$flush()

  Sys.sleep(1)

  # assert
  log1 <- elastic::Search(con, indices[[1]], body = '{"query": {"match_all": {}} }')$hits$hits
  log2 <- elastic::Search(con, indices[[2]], body = '{"query": {"match_all": {}} }')$hits$hits
  expect_identical(log1[[1]][["_source"]][["msg"]], msg1)
  expect_identical(log2[[1]][["_source"]][["msg"]], msg2)
})


test_that("AppenderElasticSearch - with create body provided - creates index with correct data types", {

  # arrange
  con <- elastic::connect(elastic_ip)

  index <- "test_logging_r_1"
  body <-
  '
  {
    "mappings": {
      "_source": {
          "includes": ["*"]
      },
      "properties": {
        "caller": {
          "type": "text",
          "fields": {
            "keyword": {
              "type": "keyword",
              "ignore_above": 256
            }
          }
        },
        "error": {
          "type": "text",
          "fields": {
            "keyword": {
              "type": "keyword",
              "ignore_above": 256
            }
          }
        },
        "level": {
          "type": "long"
        },
        "logger": {
          "type": "text",
          "fields": {
            "keyword": {
              "type": "keyword",
              "ignore_above": 256
            }
          }
        },
        "msg": {
          "type": "text",
          "fields": {
            "keyword": {
              "type": "keyword",
              "ignore_above": 256
            }
          }
        },
        "timestamp": {
          "type": "text",
          "fields": {
            "keyword": {
              "type": "keyword",
              "ignore_above": 256
            }
          }
        },
        "foobar": {
          "type": "text"
        }
      }
    }
  }
'

  jsonlite::toJSON(body)

  on.exit({
    try(elastic::index_delete(con, index))
    lg$config(NULL)
  })

  app <- AppenderElasticSearch$new(
    con,
    index,
    index_create_body = body
  )

  lg <-
    get_logger("test/AppenderElasticSearch")$
    add_appender(app)$
    set_propagate(FALSE)$
    set_threshold(NA)

  # act
  msg1 <- "this should end up in index-1"
  lg$fatal(msg1, error = "404")
  app$flush()
  Sys.sleep(1)

  # assert
  mappings <- elastic::mapping_get(con, index)
  expect_identical(mappings$test_logging_r_1$mappings$properties$foobar$type, "text")
})
