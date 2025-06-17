context("AppenderDt")


x <- LogEvent$new(
  logger = Logger$new("dummy"),
  level = 200L,
  timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
  caller = NA_character_,
  msg = "foo bar"
)




test_that("AppenderDynatrace: appending works", {
  # arrange
  app <- AppenderDynatrace$new(url = "http://foo.bar", api_key = "hashbaz")
  y <- x$clone()
  y$level <- "warn"

  httr_response_mock <- function(req) {
    sent_request <<- req
    httr2::response(status_code = 204)
  }

  httr2::local_mocked_responses(httr_response_mock)

  # act
  app$append(y)

  # assert
  sent_body <- sent_request$body$data

  sent_body

  expect_identical(
    sent_body,
    "[{\"level\":\"warn\",\"timestamp\":\"2018-11-02 17:19:33\",\"logger\":\"dummy\",\"caller\":null,\"content\":\"foo bar\"}]")

  expect_identical(sent_request$headers[["Content-Type"]], "application/json")
  expect_identical(sent_request$headers[["Authorization"]], "Api-Token hashbaz")
})
