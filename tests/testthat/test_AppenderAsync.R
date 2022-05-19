AppenderSleepyFile <- R6::R6Class(
  "AppenderSleepyFile",
  inherit = lgr::AppenderFile,
  public = list(
    append = function(event){
      for (i in 1:2){
        Sys.sleep(1)
      }
      cat(
        private$.layout$format_event(event),
        sep = "\n",
        file = get(".file", envir = private),
        append = TRUE
      )
    }
  )
)


test_that("AppenderAsync works [sequential]", {
  old_plan <- future::plan()
  future::plan("sequential")
  sleepAppender <- AppenderSleepyFile$new(file = tempfile())
  asyncAppender <- AppenderAsync$new(sleepAppender)

  lg <- get_logger("a/test/logger")$
    set_appenders(list(async = asyncAppender))$
    set_propagate(FALSE)

  on.exit({
    unlink(readLines(sleepAppender$file))
    future::plan(old_plan)
    lg$config(NULL)
  })

  on.exit(lg$config(NULL))

  lg$info("sleepy test")

  expect_length(readLines(sleepAppender$file), 1)
  expect_match(readLines(sleepAppender$file), "sleepy test")
})


for (plan_test in c("multisession", "multicore")){

  test_that(sprintf("AppenderAsync works with file [%s]", plan_test), {

    if (!future::supportsMulticore() && identical(plan_test, "multicore")) {
      skip("'multicore' is not supported on system")
    }

    old_plan <- future::plan()
    future::plan(plan_test)
    sleepAppender <- AppenderSleepyFile$new(file = tempfile())
    asyncAppender <- AppenderAsync$new(sleepAppender)

    lg <- get_logger("a/test/logger")$
      set_appenders(list(async = asyncAppender))$
      set_propagate(FALSE)

    on.exit({
      unlink(readLines(sleepAppender$file))
      future::plan(old_plan)
      lg$config(NULL)
    })

    lg$info("sleepy test 1")
    lg$info("sleepy test 2")
    lg$info("sleepy test 3")

    # appender is sleeping in background
    expect_false(asyncAppender$resolved)
    expect_length(readLines(sleepAppender$file), 0)

    Sys.sleep(3)

    # appender should have resolved now
    expect_true(asyncAppender$resolved)
    expect_length(readLines(sleepAppender$file), 3)
    expect_match(readLines(sleepAppender$file), "sleepy test")
  })



  test_that(sprintf("AppenderAsync$prune_futures() works [%s]", plan_test), {
    # implicitely test $prune_futures() which is called inside $append() which
    # is called by the logger

    if (!future::supportsMulticore() && identical(plan_test, "multicore")) {
      skip("'multicore' is not supported on system")
    }

    old_plan <- future::plan()
    future::plan(plan_test)
    sleepAppender <- AppenderSleepyFile$new(file = tempfile())
    asyncAppender <- AppenderAsync$new(sleepAppender)

    on.exit({
      unlink(readLines(sleepAppender$file))
      future::plan(old_plan)
      lg$config(NULL)
    })

    lg <- get_logger("a/test/logger")$
      set_appenders(list(async = asyncAppender))$
      set_propagate(FALSE)


    lg$info("sleepy test 1")
    lg$info("sleepy test 2")
    lg$info("sleepy test 3")

    expect_length(asyncAppender$futures, 3)
    for (el in asyncAppender$futures){
      expect_s3_class(el, "Future")
    }

    Sys.sleep(3)

    # antoher call to $append should clear the no-finished futures
    lg$info("sleepy test 4")

    expect_length(asyncAppender$futures, 1)
    for (el in asyncAppender$futures){
      expect_s3_class(el, "Future")
    }

    Sys.sleep(3)

    # everything should have been written to file
    expect_length(readLines(sleepAppender$file), 4)
  })
}
