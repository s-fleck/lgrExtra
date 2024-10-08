# AppenderDt ----------------------------------------------------------

#' Log to an in-memory data.table
#'
#' @description
#'
#' An Appender that outputs to an in-memory `data.table`. It fulfill a similar
#' purpose as the more flexible [lgr::AppenderBuffer] and is mainly included for
#' historical reasons/backwards compatibility with older version of **lgr**.
#'
#' **NOTE**: AppenderDt has been superseded by [lgr::AppenderBuffer] and is
#' kept mainly for archival purposes.
#'
#' @section Custom Fields:
#'
#' `AppenderDt` supports [lgr::custom fields][lgr::LogEvent], but they have to be
#' pre-allocated in the `prototype` argument. Custom fields that are not
#' part of the prototype are inserted in the list-column `.fields` if it
#' exists.
#'
#'
#' @section Creating a Data Table Appender:
#'
#' In addition to the usual fields, `AppenderDt$new()` requires that you supply
#' a `buffer_size` and a `prototype`. These determine the structure of the
#' `data.table` used to store the log this appender creates and cannot be
#' modified anymore after the instantiation of the appender.
#'
#' The [lgr::Layout] for this Appender is used only to format console output of
#' its `$show()` method.
#'
#'
#' @section Comparison AppenderBuffer and AppenderDt:
#'
#' Both [lgr::AppenderBuffer] and [AppenderDt] do in memory buffering of events.
#' AppenderBuffer retains a copies of the events it processes and has the
#' ability to pass the buffered events on to other Appenders. AppenderDt
#' converts the events to rows in a `data.table` and is a bit harder to
#' configure. Used inside loops (several hundred iterations),
#' AppenderDt has much less overhead than AppenderBuffer. For single logging
#' calls and small loops, AppenderBuffer is more performant. This is related to
#' how memory pre-allocation is handled by the appenders.
#'
#' @template appender
#' @export
#' @seealso [data.table::data.table]
#' @examples
#' lg <- lgr::get_logger("test")
#' lg$config(list(
#'   appenders = list(memory = AppenderDt$new()),
#'   threshold = NA,
#'   propagate = FALSE  # to prevent routing to root logger for this example
#' ))
#' lg$debug("test")
#' lg$error("test")
#'
#' # Displaying the log
#' lg$appenders$memory$data
#' lg$appenders$memory$show()
#' lgr::show_log(target = lg$appenders$memory)
#'
#' # If you pass a Logger to show_log(), it looks for the first AppenderDt
#' # that it can find.
#' lgr::show_log(target = lg)
#'
#' # Custom fields are stored in the list column .fields by default
#' lg$info("the iris data frame", caps = LETTERS[1:5])
#' lg$appenders$memory$data
#' lg$appenders$memory$data$.fields[[3]]$caps
#' lg$config(NULL)
#' @export
AppenderDt <- R6::R6Class(
  "AppenderDt",
  inherit = Appender,
  cloneable = FALSE,
  public = list(

    #' @description Creating a new AppenderDt
    #'
    #' @param buffer_size `integer` scalar. Number of rows of the in-memory `data.table`
    #' @param prototype A prototype `data.table`. The prototype must be a
    #'     `data.table` with the same columns and column types as the data
    #'     you want to log. The actual content of the columns is irrelevant.
    #'     There are a few reserved column names that have special meaning:
    #'     * `.id`: `integer` (mandatory). Must always be the first column
    #'         and is used internally by the Appender
    #'     * `.fields`: `list` (optional). If present all custom values of the
    #'         event (that are not already part of the prototype) are stored in
    #'         this list column.
    initialize = function(
      threshold = NA_integer_,
      layout = LayoutFormat$new(
        fmt = "%L [%t] %m %f",
        timestamp_fmt = "%H:%M:%OS3",
        colors = getOption("lgr.colors", list())
      ),

      # the column names .id and .fields are hardcoded in lgr::as_event_list.data.frame
      prototype = data.table::data.table(
        .id  = NA_integer_,
        level = NA_integer_,
        timestamp = Sys.time(),
        logger = NA_character_,
        caller = NA_character_,
        msg = NA_character_,
        .fields = list(list())
      ),
      buffer_size = 1e5,
      filters = NULL
    ){
      assert_namespace("data.table")
      assert(is_scalar_integerish(buffer_size))
      assert(
        data.table::is.data.table(prototype) && is.integer(prototype$.id),
        "'prototype' must be a data.table with an integer column '.id'"
      )

      if (".fields" %in% names(prototype) && !is.list(prototype$.fields)){
        warning(
          "`prototype` has the special column `.fields` but it is ",
          class_fmt(prototype$.fields), " instead of a list-column. ",
          "Coercing to list-column."
        )
      }

      private$current_row <- 0L
      private$id <- 0L
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)

      # initialize empty dt
      prototype <- data.table::copy(prototype)
      for (j in seq_along(prototype)){
        if (is.list(prototype[[j]])){
          data.table::set(prototype, i = 1L, j = j, value = list(list(NULL)))
        } else {
          data.table::set(prototype, i = 1L, j = j, value = NA)
        }
      }
      dd <- list(
        prototype,
        list(.id = rep(prototype[[1]], buffer_size - 1L))
      )
      private$.data <- data.table::rbindlist(
        dd,
        fill = TRUE
      )

      # store names list columsn for use in append()
      list_cols <- vapply(private$.data, is.list, logical(1))
      private$list_cols <- names(list_cols[list_cols])

      invisible(self)
    },


    append = function(
      event
    ){
      # AppenderDt is designed for minimum overhead, so it does not use a
      # Layout for transforming the log event into a tabular structure but
      # rather the process is hardcoded
      dt <- get(".data", private)
      datanames <- names(dt)
      valnames  <- setdiff(datanames, ".id")

      # Select and prepare event values to be inserted into data
      vals <- event[["values"]]

      # handle .fields
      if (".fields" %in% datanames){
        vals[[".fields"]] <- vals[!names(vals) %in% valnames]
      }

      vals <- vals[valnames]
      names(vals) <- valnames


      # handle list-columns
      vals[vapply(vals, is.null, FALSE)] <- list(NULL)
      list_cols <- get("list_cols", private)
      vals[list_cols] <- lapply(vals[list_cols], list)

      # Prepare values for vectorized insert (if necessary)
      lengths <- vapply(vals, length, 1L, USE.NAMES = FALSE)
      lenmax  <- max(lengths)
      assert(all(lengths %in% c(1, lenmax)))

      # take special care if vectorized insert is bigger than buffer size
      if (lenmax > nrow(dt)){
        vals <- lapply(vals, trim_to_buffer_size, nrow(dt))
        private[["id"]] <- get("id", envir = private) + lenmax - nrow(private$.data)
        lenmax <- nrow(dt)
      }
      i   <- seq_len(lenmax)

      # generate new ids
      ids <- i + get("id", private)

      # check if rotation is necessary
      if (get("current_row", private) + lenmax <= nrow(dt)){
        i   <- i + get("current_row", envir = private)
        private[["current_row"]] <- get("current_row", envir = private) + lenmax
      } else {
        # rotate buffer
        assign("current_row", lenmax, envir = private)
      }

      # Perform the insert
      data.table::set(
        dt,
        i,
        j = c(".id", names(vals)),
        value = c(list(ids), vals)
      )

      private[["id"]] <- get("id", envir = private) + lenmax
    },

    show = function(
      threshold = NA_integer_,
      n = 20L
    ){
      assert(is_scalar_integerish(n))
      threshold <- standardize_threshold(threshold)

      if (is.na(threshold)) threshold <- Inf
      dd <- self$dt

      if (identical(nrow(dd),  0L)){
        cat("[empty log]")
        return(invisible(NULL))
      }

      res <- tail(dd[dd$level <= threshold, ], n)

      # construct a hackish pseudo-log event out of the data.table. This is
      # guranteed to work with LayoutFormat, other layouts might run into
      # issues
      walk(
        as_event_list(res),
        function(.x){
          cat(self$layout$format_event(.x), "\n", sep = "")
        }
      )

      invisible(res)
    },

    set_layout = function(layout){
      assert(inherits(layout, "Layout"))
      if (!inherits(layout, "LayoutFormat")){
        warning(
          "AppenderDt currently only fully supports LayoutFormat. Accessing",
          "event$values or event$.logger from other Layouts is not possible.",
          "if you run into issues, don't hesitate to file a bug report",
          "or feature request on github."
        )
      }
      private$.layout <- layout
      invisible(self)
    }
  ),



  # +- active ---------------------------------------------------------------
  active = list(

    #' @description
    #' Get the log recorded by this `Appender` as a `data.table` with a maximum
    #' of `buffer_size` rows
    dt = function(){
      tmp <- private$.data[!is.na(private$.data$.id)]
      tmp[order(tmp$.id), ]
    },

    data = function(){
      as.data.frame(self$dt)
    },

    destination = {
      function() "in memory data.table"
    }
  ),


  private = list(
    id = NULL,
    current_row = NULL,
    .data = NULL,
    list_cols = NULL
  )
)




# trim multi-valued events from vectorized inserts to the buffer size
trim_to_buffer_size <- function(x, buffer_size){
  if (length(x) <= buffer_size)
    x
  else
    x[seq.int(length(x) - buffer_size + 1L, length(x))]
}
