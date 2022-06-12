# is called by the logger
pkgload::load_all()

# funs --------------------------------------------------------------------

AppenderSleepyFile <- R6::R6Class(
  "AppenderSleepyFile",
  inherit = lgr::AppenderFile,
  public = list(
    append = function(event){
      Sys.sleep(0.3)
      cat(
        private$.layout$format_event(event),
        sep = "\n",
        file = get(".file", envir = private),
        append = TRUE
      )
    }
  )
)



AppenderFuture <- R6::R6Class(
  "AppenderFuture",
  inherit = lgr::Appender,
  public = list(

    #' @description
    #' @param appender a single `Appender` that will be called asynchronously
    initialize = function(
    appender
    ){
      self$set_appender(appender)
      self
    },

    set_appender = function(appender){
      private[[".appender"]] <- appender
    },

    append = function(event){
      future::future(private[[".appender"]][["append"]](event))
    }
  ),

  # +- active ---------------------------------------------------------------
  active = list(
    appender = function() private[[".appender"]]
  ),

  private = list(
    .appender = NULL
  )
)


AppenderPromise <- R6::R6Class(
  "AppenderPromise",
  inherit = lgr::Appender,
  public = list(

    #' @description
    #' @param appender a single `Appender` that will be called asynchronously
    initialize = function(
    appender
    ){
      self$set_appender(appender)
      self
    },

    set_appender = function(appender){
      private[[".appender"]] <- appender
    },

    append = function(event){
      promises::promise(~private[[".appender"]][["append"]](event))
    }
  ),

  # +- active ---------------------------------------------------------------
  active = list(
    appender = function() private[[".appender"]]
  ),

  private = list(
    .appender = NULL
  )
)



AppenderFuturePromise <- R6::R6Class(
  "AppenderPromise",
  inherit = lgr::Appender,
  public = list(

    #' @description
    #' @param appender a single `Appender` that will be called asynchronously
    initialize = function(
    appender
    ){
      self$set_appender(appender)
      self
    },

    set_appender = function(appender){
      private[[".appender"]] <- appender
    },

    append = function(event){
      promises::future_promise(~private[[".appender"]][["append"]](event))
    }
  ),

  # +- active ---------------------------------------------------------------
  active = list(
    appender = function() private[[".appender"]]
  ),

  private = list(
    .appender = NULL
  )
)


# logic -------------------------------------------------------------------

old_plan <- future::plan()
future::plan("multisession")


asyncAppender   <- AppenderAsync$new(AppenderSleepyFile$new(file = tempfile()))  # current implementation
futureAppender <- AppenderFuture$new(AppenderSleepyFile$new(file = tempfile()))
promiseAppender  <- AppenderPromise$new(AppenderSleepyFile$new(file = tempfile()))
futurePromiseAppender <- AppenderFuturePromise$new(AppenderSleepyFile$new(file = tempfile()))


lgSequential <- get_logger("a/test/logger/sequential")$
  set_appenders(AppenderSleepyFile$new(file = tempfile()))$
  set_propagate(FALSE)

lgAsync <- get_logger("a/test/logger/async")$
  set_appenders(asyncAppender)$
  set_propagate(FALSE)

lgFuture <- get_logger("a/test/logger/future")$
  set_appenders(futureAppender)$
  set_propagate(FALSE)

lgPromise <- get_logger("a/test/logger/promise")$
  set_appenders(promiseAppender)$
  set_propagate(FALSE)

lgFuturePromise <- get_logger("a/test/logger/futurePromise")$
  set_appenders(futurePromiseAppender)$
  set_propagate(FALSE)


bm <- bench::mark(
  async = lgAsync$info("sleepy test 1"),
  # future = lgFuture$info("sleepy test 1"),
  # promise = lgPromise$info("sleepy test 1"),
  # futurePromise = lgFuturePromise$info("sleepy test 1"),
  sequential = lgSequential$info("normal test 1"),
  check = FALSE,
  iterations = 100
)

print(plot(bm))


res <- list(
  sequential = readLines(lgSequential$appenders[[1]]$file),
  async = readLines(lgAsync$appenders[[1]]$appender$file)
  # future = readLines(lgFuture$appenders[[1]]$appender$file)
  # promise = readLines(lgPromise$appenders[[1]]$appender$file),
  # futurePromise = readLines(lgFuturePromise$appenders[[1]]$appender$file)
)


print(sapply(res, length))
