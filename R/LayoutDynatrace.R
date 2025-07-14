
# LayoutDbi ---------------------------------------------------------------


#' Format log events for output to Dynatrace
#'
#' Similar to [lgr::LayoutJson], but with some modifications to prepare data
#' for Dynatrace.
#'
#' @template layout
#'
#' @seealso [transform_event_dynatrace()], \url{https://docs.dynatrace.com/docs/discover-dynatrace/references/semantic-dictionary/fields#service}
#' @export
LayoutDynatrace <- R6::R6Class(
  "LayoutDynatrace",
  inherit = lgr::Layout,
  public = list(
    initialize = function(
      toJSON_args = list(auto_unbox = TRUE),
      transform_event = transform_event_dynatrace
    ){
      self$set_transform_event(transform_event)
      self$set_toJSON_args(toJSON_args)
      self
    },

    format_event = function(event) {
      values <- get(".transform_event", private)(event)

      do.call(
        jsonlite::toJSON,
        args = c(list(x = values), get(".toJSON_args", private))
      )
    },

    set_toJSON_args = function(x){
      assert(is.list(x))
      assert(identical(length(names(x)), length(x)))
      private[[".toJSON_args"]] <- x
      invisible(self)
    },

    set_transform_event = function(x){
      assert(
        is.function(x) &&
        identical(names(formals(x)), "event"),
        "`transform_event` must be a function with a single argument `event`"
      )

      private[[".transform_event"]] <- x
      invisible(self)
    }
  ),


  active = list(
    #' @field toJSON_args a list of values passed on to [jsonlite::toJSON()]
    toJSON_args   = function() get(".toJSON_args", private),

    #' @field transform_event a `function` with a single argument `event` that
    #'   takes a [lgr::LogEvent] and returns a `list`.
    transform_event = function() get(".transform_event", private)
  ),


  private = list(
    .toJSON_args = NULL,
    .transform_event = NULL
  )
)


#' Transform a log event for Dynatrace
#'
#' @param event a [lgr::LogEvent] object.
#'
#' @returns a `list` of values that will be serialized to JSON for Dynatrace.
#' @seealso \url{https://docs.dynatrace.com/docs/discover-dynatrace/references/semantic-dictionary/fields#service}
#' @export
transform_event_dynatrace <- function(event) {
  values <- get("values", event)

  values[["logger"]] <- gsub("/", ".", values[["logger"]], fixed = TRUE)
  values[["level"]] <- unname(lgr::label_levels(values[["level"]]))
  values[["log.raw_level"]] <- values[["level"]]

  value_names <- names(values)

  # hard coded loop for maximum performance at the cost of readability
  for (i in seq_along(value_names)) {
    if (value_names[i] == "msg") {
      value_names[i] <- "content"

    } else if (value_names[i] == "level") {
      value_names[i] <- "loglevel"

    } else if (value_names[i] == "logger") {
      value_names[i] <- "log.logger"

    } else if (value_names[i] == "caller") {
      value_names[i] <- "code.function"

    } else if (value_names[i] == "rawMsg") {  # TODO
      value_names[i] <- "content_raw"
    }
  }

  names(values) <- value_names

  values
}
