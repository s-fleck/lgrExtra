# is called by the logger

old_plan <- future::plan()
future::plan("multisession")

sleepAppender <- AppenderSleepyFile$new(file = tempfile())
asyncAppender <- AppenderAsync$new(AppenderFile$new(file = tempfile()))


lgAsync <- get_logger("a/test/logger/async")$
  set_appenders(asyncAppender)$
  set_propagate(FALSE)

lgSequential <- get_logger("a/test/logger/sequential")$
  set_appenders(AppenderFile$new(file = tempfile()))$
  set_propagate(FALSE)


bm <- bench::mark(
  async = lgAsync$info("sleepy test 1"),
  sequential = lgSequential$info("normal test 1"), check = FALSE, iterations = 100
)

plot(
  bm)



readLines(lgAsync$appenders[[1]]$appender$file)
