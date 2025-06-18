test_that("set minimal setting in initialisation", {
  mock_create_log_group <- mock()
  mock_put_retention_policy <- mock()
  mock_put_log_events <- mock()
  mock_get_log_events <- mock()

  mock_paws_management_cwl <- mock(
    list(
      create_log_group = mock_create_log_group,
      put_retention_policy = mock_put_retention_policy,
      put_log_events = mock_put_log_events,
      get_log_events = mock_get_log_events
    )
  )

  local_mocked_bindings(
    cloudwatchlogs = mock_paws_management_cwl,
    .package = "paws.management"
  )
  local_mocked_bindings(
    Sys.Date = function() "2023-10-20",
    sample = function(...) "abcde123",
    .package = "base"
  )

  app <- AppenderAWSCloudWatchLog$new(
    "lgrExtra"
  )

  private <- app$.__enclos_env__$private

  expect_equal(mock_arg(mock_paws_management_cwl), list(list()))
  expect_equal(mock_arg(mock_create_log_group), list(logGroupName = "lgrExtra"))
  expect_equal(private$.flush_threshold, 200)
  expect_equal(private$.flush_on_rotate, TRUE)
  expect_equal(private$.flush_on_exit, TRUE)

  expect_equal(private$.log_group_name, "lgrExtra")
  expect_equal(private$.log_stream_name, "lgrExtra/2023-10-20/abcde123")
  expect_equal(private$.layout, lgr::LayoutFormat$new(fmt = "%L: %m", colors = list()))
  expect_equal(private$.log_group_retention_days, NULL)
})

test_that("set minimal setting in initialisation", {
  mock_create_log_group <- mock()
  mock_put_retention_policy <- mock()
  mock_put_log_events <- mock()
  mock_get_log_events <- mock()

  mock_paws_management_cwl <- mock(
    list(
      create_log_group = mock_create_log_group,
      put_retention_policy = mock_put_retention_policy,
      put_log_events = mock_put_log_events,
      get_log_events = mock_get_log_events
    )
  )

  local_mocked_bindings(
    cloudwatchlogs = mock_paws_management_cwl,
    .package = "paws.management"
  )
  local_mocked_bindings(
    Sys.time = function() 123456,
    sample = function(...) "abcde123",
    .package = "base"
  )

  l1 <- lgr::LayoutFormat$new(fmt = "%L: %m", colors = list())

  app <- AppenderAWSCloudWatchLog$new(
    log_group_name = "lgrExtra",
    log_stream_name = "foobar",
    log_group_retention_days = 30,
    paws_config = list(var1 = "zoo"),
    threshold = 2,
    layout = l1,
    buffer_size = 0,
    flush_threshold = "warn",
    flush_on_exit = TRUE,
    flush_on_rotate = TRUE
  )

  private <- app$.__enclos_env__$private

  expect_equal(mock_arg(mock_paws_management_cwl), list(list(var1 = "zoo")))
  expect_equal(mock_arg(mock_create_log_group), list(logGroupName = "lgrExtra"))
  expect_equal(mock_arg(mock_put_retention_policy), list(logGroupName = "lgrExtra", retentionInDays = 30))
  expect_equal(private$.flush_threshold, 300)
  expect_equal(private$.flush_on_rotate, TRUE)
  expect_equal(private$.flush_on_exit, TRUE)

  expect_equal(private$.log_group_name, "lgrExtra")
  expect_equal(private$.log_stream_name, "foobar/abcde123")
  expect_equal(private$.layout, l1)
  expect_equal(private$.log_group_retention_days, 30)
})

test_that("check logs are correctly formatted to send to cloudwatch", {
  mock_create_log_group <- mock(cycle = T)
  mock_put_retention_policy <- mock(cycle = T)
  mock_put_log_events <- mock(list(nextSequenceToken = "123"), cycle = T)
  mock_get_log_events <- mock(cycle = T)

  mock_paws_management_cwl <- mock(
    list(
      create_log_group = mock_create_log_group,
      put_retention_policy = mock_put_retention_policy,
      put_log_events = mock_put_log_events,
      get_log_events = mock_get_log_events
    ), cycle = T
  )

  local_mocked_bindings(
    cloudwatchlogs = mock_paws_management_cwl,
    .package = "paws.management"
  )
  local_mocked_bindings(
    Sys.time = function() 123456,
    sample = function(...) "abcde123",
    .package = "base"
  )

  app <- AppenderAWSCloudWatchLog$new(
    "lgrExtra"
  )

  lg <- lgr::get_logger("lgrExtra")$add_appender(app)$set_propagate(FALSE)
  lg$info("FOO")
  lg$info("BAR")

  expect_equal(mock_args(mock_put_log_events), list(
    list(
      logGroupName = "lgrExtra",
      logStreamName = "lgrExtra/1970-01-02/abcde123",
      logEvents = list(
        list(
          timestamp = 123456000,
          message = "INFO : FOO"
        )
      )
    ),
    list(
      logGroupName = "lgrExtra",
      logStreamName = "lgrExtra/1970-01-02/abcde123",
      logEvents = list(
        list(
          timestamp = 123456000,
          message = "INFO : BAR"
        )
      ),
      sequenceToken = "123"
    )
  ))
})

test_that("check batch logs are correctly formatted to send to cloudwatch", {
  mock_create_log_group <- mock(cycle = T)
  mock_put_retention_policy <- mock(cycle = T)
  mock_put_log_events <- mock(list(nextSequenceToken = "123"), cycle = T)
  mock_get_log_events <- mock(cycle = T)

  mock_paws_management_cwl <- mock(
    list(
      create_log_group = mock_create_log_group,
      put_retention_policy = mock_put_retention_policy,
      put_log_events = mock_put_log_events,
      get_log_events = mock_get_log_events
    ), cycle = T
  )

  local_mocked_bindings(
    cloudwatchlogs = mock_paws_management_cwl,
    .package = "paws.management"
  )
  local_mocked_bindings(
    Sys.time = function() 123456,
    sample = function(...) "abcde123",
    .package = "base"
  )

  app <- AppenderAWSCloudWatchLog$new(
    "lgrExtra", buffer_size = 1
  )

  lg <- lgr::get_logger("lgrExtra")$add_appender(app)$set_propagate(FALSE)
  lg$info("FOO")
  lg$info("BAR")
  lg$info("CHO")
  lg$info("ZOO")

  expect_equal(mock_args(mock_put_log_events), list(
    list(
      logGroupName = "lgrExtra",
      logStreamName = "lgrExtra/1970-01-02/abcde123",
      logEvents = list(
        list(
          timestamp = 123456000,
          message = "INFO : FOO"
        ),
        list(
          timestamp = 123456000,
          message = "INFO : BAR"
        )
      )
    ),
    list(
      logGroupName = "lgrExtra",
      logStreamName = "lgrExtra/1970-01-02/abcde123",
      logEvents = list(
        list(
          timestamp = 123456000,
          message = "INFO : CHO"
        ),
        list(
          timestamp = 123456000,
          message = "INFO : ZOO"
        )
      ),
      sequenceToken = "123"
    )
  ))
})

test_that("check batch logs are correctly formatted to send to cloudwatch", {
  mock_create_log_group <- mock(cycle = T)
  mock_put_retention_policy <- mock(cycle = T)
  mock_put_log_events <- mock(list(nextSequenceToken = "123"), cycle = T)
  mock_get_log_events <- mock(cycle = T)

  mock_paws_management_cwl <- mock(
    list(
      create_log_group = mock_create_log_group,
      put_retention_policy = mock_put_retention_policy,
      put_log_events = mock_put_log_events,
      get_log_events = mock_get_log_events
    ), cycle = T
  )

  local_mocked_bindings(
    cloudwatchlogs = mock_paws_management_cwl,
    .package = "paws.management"
  )
  local_mocked_bindings(
    Sys.time = function() 123456,
    sample = function(...) "abcde123",
    .package = "base"
  )

  app <- AppenderAWSCloudWatchLog$new(
    "lgrExtra", buffer_size = 1
  )

  lg <- lgr::get_logger("lgrExtra")$add_appender(app)$set_propagate(FALSE)
  lg$info("FOO")
  lg$info("BAR")
  lg$info("CHO")
  lg$info("ZOO")

  expect_equal(mock_args(mock_put_log_events), list(
    list(
      logGroupName = "lgrExtra",
      logStreamName = "lgrExtra/1970-01-02/abcde123",
      logEvents = list(
        list(
          timestamp = 123456000,
          message = "INFO : FOO"
        ),
        list(
          timestamp = 123456000,
          message = "INFO : BAR"
        )
      )
    ),
    list(
      logGroupName = "lgrExtra",
      logStreamName = "lgrExtra/1970-01-02/abcde123",
      logEvents = list(
        list(
          timestamp = 123456000,
          message = "INFO : CHO"
        ),
        list(
          timestamp = 123456000,
          message = "INFO : ZOO"
        )
      ),
      sequenceToken = "123"
    )
  ))
})

test_that("check get logs from cloudwatch", {
  mock_create_log_group <- mock(cycle = T)
  mock_put_retention_policy <- mock(cycle = T)
  mock_put_log_events <- mock(list(nextSequenceToken = "123"), cycle = T)
  mock_get_log_events <- mock(
    list(
      events = list(
        list(
          timestamp = 123,
          message = "INFO : FOO",
          ingestionTime = 123
        )
      ),
      nextForwardToken = "token1",
      nextBackwardToken = "token1"
    ),
    list(
      events = list(
        list(
          timestamp = 123,
          message = "INFO : BAR",
          ingestionTime = 123
        )
      ),
      nextForwardToken = "token2",
      nextBackwardToken = "token2"
    ),
    list(
      events = list(),
      nextForwardToken = "token3",
      nextBackwardToken = "token3"
    ))

  mock_paws_management_cwl <- mock(
    list(
      create_log_group = mock_create_log_group,
      put_retention_policy = mock_put_retention_policy,
      put_log_events = mock_put_log_events,
      get_log_events = mock_get_log_events
    ), cycle = T
  )

  local_mocked_bindings(
    cloudwatchlogs = mock_paws_management_cwl,
    .package = "paws.management"
  )
  local_mocked_bindings(
    Sys.time = function() 123456,
    sample = function(...) "abcde123",
    .package = "base"
  )

  app <- AppenderAWSCloudWatchLog$new(
    "lgrExtra", buffer_size = 1
  )
  expect_equal(app$data, list("INFO : FOO", "INFO : BAR"))
  expect_equal(mock_args(mock_get_log_events), list(
    list(
      logGroupName = "lgrExtra",
      logStreamName = "lgrExtra/1970-01-02/abcde123",
      startTime = 0,
      startFromHead = TRUE
    ),
    list(
      logGroupName = "lgrExtra",
      logStreamName = "lgrExtra/1970-01-02/abcde123",
      startTime = 0,
      startFromHead = TRUE,
      nextToken = "token1"
    ),
    list(
      logGroupName = "lgrExtra",
      logStreamName = "lgrExtra/1970-01-02/abcde123",
      startTime = 0,
      startFromHead = TRUE,
      nextToken = "token2"
    )
  ))
})
