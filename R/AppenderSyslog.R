# AppenderSyslog ----------------------------------------------------------

#' Log to the POSIX system log
#'
#' @description
#' An Appender that writes to the syslog on supported POSIX platforms. Requires
#' the \pkg{rsyslog} package.
#'
#' @seealso [LayoutFormat], [LayoutJson]
#' @family Appenders
#' @export
#' @examples
#' if (requireNamespace("rsyslog", quietly = TRUE)) {
#'   lg <- lgr::get_logger("rsyslog/test")
#'   lg$add_appender(AppenderSyslog$new(), "syslog")
#'   lg$info("A test message")
#'
#'   if (Sys.info()[["sysname"]] == "Linux"){
#'     system("journalctl -t 'rsyslog/test'")
#'   }
#'
#'   invisible(lg$config(NULL))  # cleanup
#' }
AppenderSyslog <- R6::R6Class(
  "AppenderSyslog",
  inherit = Appender,
  cloneable = FALSE,
  public = list(
    initialize = function(
      identifier = NULL,
      threshold = NA_integer_,
      layout = LayoutFormat$new("%m"),
      filters = NULL,
      syslog_levels = c(
        "CRITICAL" = "fatal",
        "ERR" = "error",
        "WARNING" = "warn",
        "INFO" = "info",
        "DEBUG" = "debug",
        "DEBUG" = "trace"
      )
    ){
      if (!requireNamespace("rsyslog", quietly = TRUE)) {
        stop("The 'rsyslog' package is required for this appender.")
      }
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)
      self$set_identifier(identifier)
      self$set_syslog_levels(syslog_levels)
    },


    append = function(event){
      identifier <- get(".identifier", private)
      if (is.null(identifier)) identifier <- event$logger

      rsyslog::open_syslog(identifier = identifier)
      rsyslog::set_syslog_mask("DEBUG")
      on.exit(rsyslog::close_syslog())

      rsyslog::syslog(
        private$.layout$format_event(event),
        level = private$to_syslog_levels(event$level)
      )
    },


    #' @description Define conversion between lgr and syslog log levels
    #' @param x
    #'  * a named `character` vector mapping whose names are log
    #'    levels as understood by [rsyslog::syslog()] and whose values are [lgr
    #'    log levels][log_levels] (either `character` or `numeric`)
    #'  * a `function` that takes a vector of lgr log levels as input and
    #'    returns a `character` vector of log levels for [rsyslog::syslog()].
    set_syslog_levels = function(x){
      if (is.function(x)){
        private$.syslog_levels <- x
      } else {
        assert(all_are_distinct(unname(x)))
        assert(is_equal_length(x, names(x)))
        private$.syslog_levels <- structure(
          standardize_log_levels(unname(x)),
          names = names(x)
        )
      }

      self
    },

    #' @description Set a string to identify the process.
    set_identifier = function(x){
      assert(is.null(x) || is_scalar_character(x))
      private$.identifier <- x
      self
    }
  ),

  # +- active ---------------------------------------------------------------
  active = list(
    destination   = function() sprintf("syslog [%s]", private$.identifier),

    #' @field identifier `character` scalar. A string identifying the process;
    #'  if `NULL` defaults to the logger name
    identifier    = function() get(".identifier", private),

    #' @field syslog_levels. Either a named `character` vector or a `function`
    #'   mapping lgr [log_levels] to rsyslog log levels. See
    #'   `$set_syslog_levels()`.
    syslog_levels = function() get(".syslog_levels", private)
  ),

  private = list(
    finalize = function(){
      rsyslog::close_syslog()
    },

    to_syslog_levels = function(
      levels
    ){
      sl <- get(".syslog_levels", private)
      levels <- standardize_log_levels(levels)

      if (is.function(sl)){
        res <- sl(levels)
      } else {
        res <- names(private$.syslog_levels)[match(levels, unname(private$.syslog_levels))]
      }

      toupper(res)
    },

    .identifier = NULL,
    .syslog_levels = NULL
  )
)

