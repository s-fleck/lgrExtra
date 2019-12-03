context("SerializerJson")


test_that("SerializerJson works as expected", {

  x <- lgr::LogEvent$new(msg = "test", logger = lgr::lgr, x1 = LETTERS, x2 = "blubb", x3 = iris)
  s <- SerializerJson$new()
  s$serialize(x)

  r <- jsonlite::fromJSON(s$serialize(x))
  expect_true("x1" %in% names(r))
  expect_false("x3" %in% names(r))


})
