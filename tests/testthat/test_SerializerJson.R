context("SerializerJson")


test_that("SerializerJson works as expected", {
  x <- lgr::LogEvent$new(msg = "test", logger = lgr::lgr, x1 = LETTERS, x2 = "blubb", x3 = iris)
  s <- SerializerJson$new()
  s$serialize(x)

  r <- jsonlite::fromJSON(s$serialize(x))
  expect_true("x1" %in% names(r))
  expect_false("x3" %in% names(r))
})




test_that("unpack_json_cols works", {

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  lo <- LayoutDbi$new(
    col_types = c(
      level = "integer",
      timestamp = "TEXT",
      logger = "TEXT",
      caller = "TEXT",
      msg = "TEXT",
      fields = "TEXT",
      fields2 = "TEXT"
    ),

    serialized_cols = list(
      "fields"  = SerializerJson$new(),
      "fields2" = SerializerJson$new(col_filter = function(.) length(.) < 2)
    )
  )

  l <- Logger$new(
    "test/unpack_json",
    appenders = list(db = AppenderDbi$new(con, table = "unpack_json", layout = lo)),
    propagate = FALSE
  )

  l$info("blah blah", letters = letters)
  l$info("blah blah", letters = LETTERS)
  l$info("blah blah", foo = "bar", letters = "a")
  l$appenders$db$flush()


  res <- l$appenders$db$data

  unpack_json_cols(res, cols = c("fields", "fields2"))





})
