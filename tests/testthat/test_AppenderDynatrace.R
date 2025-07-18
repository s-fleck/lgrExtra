test_that("AppenderDynatrace: appending works", {
  event <- lgr::LogEvent$new(
    logger = lgr::Logger$new("dummy"),
    level = 300,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt"), tzone = "UTC"),
    caller = NA_character_,
    msg = "foo bar"
  )

  # arrange
  app <- AppenderDynatrace$new(url = "http://foo.bar", api_key = "hashbaz")


  httr_response_mock <- function(req) {
    sent_request <<- req
    httr2::response(status_code = 204)
  }

  httr2::local_mocked_responses(httr_response_mock)

  # act
  app$append(event)

  # assert
  sent_body <- sent_request$body$data

  if (utils::packageVersion("lgr") >= "0.4.5.9000") {
    expected_body <-  "[{\"loglevel\":\"warn\",\"timestamp\":\"2018-11-02 16:19:33\",\"log.logger\":\"dummy\",\"code.function\":null,\"content\":\"foo bar\",\"log.record.template\":\"foo bar\",\"log.raw_level\":\"warn\"}]"
  } else {
    expected_body <-  "[{\"loglevel\":\"warn\",\"timestamp\":\"2018-11-02 16:19:33\",\"log.logger\":\"dummy\",\"code.function\":null,\"content\":\"foo bar\",\"log.raw_level\":\"warn\"}]"
  }

  expect_identical(sent_body, expected_body)

  if (utils::packageVersion("httr2") >= "1.1.2.9000") {
    headers <- httr2::req_get_headers(sent_request, "reveal")
  } else {
    headers <- sent_request$headers
  }

  expect_identical(headers[["Content-Type"]], "application/json")
  expect_identical(headers[["Authorization"]], "Api-Token hashbaz")
})
