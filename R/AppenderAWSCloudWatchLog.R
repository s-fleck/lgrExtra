# AppenderAWSCloudWatchLog -------------------------------------------------------------


#' Log to databases via DBI
#'
#' @description
#'
#' Log to \href{https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/WhatIsCloudWatchLogs.html}{AWS CloudWatch Logs}.
#'
#'
#' @section Buffered Logging:
#'
#' By default, `AppenderAWSCloudWatchLog` writes each LogEvent which can be relatively slow.
#' To improve performance it is possible to tell
#' AppenderAWSCloudWatchLog to buffer db writes by setting `buffer_size` to something greater
#' than `0`. This buffer is written to AWS CloudWatch whenever it is full
#' (`buffer_size`), whenever a LogEvent with a level of `fatal` or `error` is
#' encountered (`flush_threshold`), or when the Appender is garbage collected
#' (`flush_on_exit`), i.e. when you close the \R session or shortly after you
#' remove the Appender object via `rm()`.
#'
#'
#' @section Creating a New Appender:
#'
#' An `AppenderAWSCloudWatchLog` is linked to an AWS Account using the
#' \href{https://www.paws-r-sdk.com/}{paws sdk package}. If the
#' log group does not exist it is created either when the Appender is first
#' instantiated or (more likely) when the first LogEvent would be written to
#' that table.
#'
#'
#' @template appender
#'
#' @examples
#' if (requireNamespace("RSQLite")){
#'   library(lgrExtra)
#'   app <- AppenderAWSCloudWatchLog$new(log_group_name = "lgrExtra")
#'   lg <- lgr::get_logger("lgrExtra")$add_appender(app)$set_propagate(FALSE)
#'   lg$info("test")
#'   print(lg$appenders[[1]]$data)
#'
#'   invisible(lg$config(NULL))  # cleanup
#' }
#' @export
AppenderAWSCloudWatchLog <- R6::R6Class(
  "AppenderAWSCloudWatchLog",
  inherit = lgr::AppenderMemory,
  cloneable = FALSE,
  public = list(

    #' @param conn,table see section *Fields*
    #' @param threshold,flush_threshold,layout,buffer_size see [AppenderBuffer]
    initialize = function(
      log_group_name,
      log_stream_name = paste(log_group_name, Sys.Date(), sep = "/"),
      log_group_retention_days = NULL,
      create_log_group = TRUE,
      paws_config = list(),
      threshold = NA_integer_,
      layout = LayoutFormat$new(fmt = "%L: %m", colors = list()),
      buffer_size = 0,
      flush_threshold = "error",
      flush_on_exit = TRUE,
      flush_on_rotate = TRUE,
      should_flush = NULL,
      filters = NULL
    ){
      assert_namespace("paws.management")

      # appender
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)

      # buffer
      private$initialize_buffer(buffer_size)

      # flush conditions
      self$set_should_flush(should_flush)
      self$set_flush_threshold(flush_threshold)
      self$set_flush_on_exit(flush_on_exit)
      self$set_flush_on_rotate(flush_on_rotate)

      # client
      self$set_client(paws.management::cloudwatchlogs(paws_config))

      # aws cloudwatch log settings
      self$set_log_group_name(log_group_name)
      self$set_log_stream_name(log_stream_name)
      self$set_log_group_retention_days(log_group_retention_days)

      if (isTRUE(create_log_group)) {
        private$.call_log("create_log_group", list(logGroupName=self$log_group_name))
      }

      if (!is.null(log_group_retention_days)) {
        private$.call_log(
          "put_retention_policy", list(logGroupName=self$log_group_name, retentionInDays=self$log_group_retention_days)
        )
      }

      return(self)
    },

    set_client = function(client){
      private$.client <- client
      invisible(self)
    },

    set_log_group_name = function(log_group_name) {
      assert(is_scalar_character(log_group_name))
      private$.log_group_name <- log_group_name
      invisible(self)
    },

    set_log_stream_name = function(log_stream_name) {
      assert(is_scalar_character(log_stream_name))

      log_stream_name_unique <- paste(
        gsub("/$", "", log_stream_name),
        private$unique_string(),
        sep = "/"
      )

      private$.log_stream_name <- log_stream_name_unique
      invisible(self)
    },

    set_log_group_retention_days = function(log_group_retention_days) {
      assert(
        is_scalar_numeric(log_group_retention_days) || is.null(log_group_retention_days)
      )
      private$.log_group_retention_days <- log_group_retention_days
    },

    flush = function(){
      lo <- get(".layout", envir = private)
      buffer <- get("buffer_events", envir = self)

      if (length(buffer)){
        logEvents = lapply(buffer, function(event) {
         list(
           timestamp = as.numeric(event$timestamp)*1000,
           message = lo$format_event(event)
         )
        })
        kwargs <- list(
          logGroupName=self$log_group_name,
          logStreamName=self$log_stream_name,
          logEvents=logEvents
        )
        kwargs[["sequenceToken"]] <- private$.log_stream_token

        resp <- tryCatch({
          private$.call_log("put_log_events", kwargs)
        }, paws_error = function(error) {
          if (paws_error_code(error) %in% c("ResourceNotFoundException")) {
            private$.call_log("create_log_stream", list(
                logGroupName = self$log_group_name,
                logStreamName=self$log_stream_name
              )
            )
            private$.call_log("put_log_events", kwargs)
          } else {
            stop(error)
          }
        })
        private$.log_stream_token <- resp$nextSequenceToken
      }

      assign("insert_pos", 0L, envir = private)
      private$.buffer_events <- list()
      invisible(self)
    }
  ),


  # +- active ---------------------------------------------------------------
  active = list(
    #' @field client a [paws.management cloudwatchlogs client][paws.management::cloudwatchlogs]
    client = function() {
      private$.client
    },

    log_group_name = function(){
      private$.log_group_name
    },

    log_stream_name = function(){
      private$.log_stream_name
    },

    data = function(){
      event_count <- 1

      kwargs <- list(
        logGroupName = self$log_group_name,
        logStreamName = self$log_stream_name,
        startTime = 0,
        startFromHead = TRUE
      )
      events <- list()
      while (event_count > 0) {

        response <- private$.call_log("get_log_events", kwargs)

        event_count <- length(response$events)
        position <- length(events) + 1
        if (event_count) {
          kwargs$nextToken <- response$nextForwardToken
          events[[position]] <- response$events
        }
        if (event_count == 0) break
      }
      resp <- unlist(events, recursive = FALSE)
      return(
        lapply(resp, function(l) trimws(l$message, which = "right"))
      )
    }
  ),

  # +- private -------------------------------------------------------------
  private = list(
    finalize = function() {
      if (self$flush_on_exit)
        self$flush()
    },

    .call_log = function(method, kwargs) {
      tryCatch({
        do.call(private$.client[[method]], kwargs)
      }, paws_error = function(error) {
        if (paws_error_code(error) %in% ignore_error_codes) {
          return(NULL)
        }
        stop(error)
      })
    },

    unique_string = function(){
      paste(sample(c(1:9, letters), size = 8, replace = T), collapse = "")
    },

    .client = NULL,
    .log_group_name = NULL,
    .log_stream_name = NULL,
    .log_group_retention_days = NULL,
    .log_stream_token = NULL
  )
)

# helpers -----------------------------------------------------------------

ignore_error_codes <- c("ResourceAlreadyExistsException", "OperationAbortedException")

paws_error_code <- function(error) {
  error_response <- error[["error_response"]]
  if (is.null(error_response[["Code"]])) {
    error_response[["__type"]]
  } else {
    error_response[["Code"]]
  }
}
