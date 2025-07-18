
# LayoutDbi ---------------------------------------------------------------


#' Format log events for output to Dynatrace
#'
#' @description
#' Similar to [lgr::LayoutJson], but with some modifications to prepare data
#' for Dynatrace. In theory it should be flexible enough to be-rused for other
#' HTTP-API use-cases as well.
#'
#' This Layout provides 3 ways to transform the event before ingestion into
#' Dynatrace:
#'
#' 1. `transform_event` a generic function to transform the event
#' 2. `transform_event_names` a named `character` vector or a second
#'     function to rename fields
#' 3. `excluded_fields` a `character` vector to include fields.
#'
#' In theory supplying a custom `transform_event` function is enough to
#' transform the event, but the other two parameters are provided for
#' convenience. Please note that they are applied in order (e.g. if you
#' rename a field you have to exclude the *renamed* field to really exclude it).
#'
#' @template layout
#'
#'
#' @seealso \url{https://docs.dynatrace.com/docs/discover-dynatrace/references/semantic-dictionary/fields#service}
#' @export
LayoutDynatrace <- R6::R6Class(
  "LayoutDynatrace",
  inherit = lgr::Layout,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param toJSON_args a list of arguments passed to [jsonlite::toJSON()],
    #'
    #' @param transform_event a `function` with the arguments `event`,
    #'   `transform_event_names` and `excluded_fields`. See
    #'   [transform_event_dynatrace()] for an example.
    #'
    #' @param excluded_fields A `character` vector of field names to exclude
    #'   from the final output. passed to [transform_event()].
    #'
    #' @param transform_event_names A named `character` vector mapping original
    #'   field names to Dynatrace-compatible ones, or a function with a single
    #'   mandatory argument that accepts a character vector of field names.
    #'   passed to [transform_event()].
    initialize = function(
      transform_event = transform_event_dynatrace,
      transform_event_names = c(
        "msg" = "content",
        "level" = "loglevel",
        "logger" = "log.logger",
        "caller" = "code.function",
        "rawMsg" = "log.record.template"  # inspired by https://github.com/open-telemetry/semantic-conventions/issues/2064
      ),
      excluded_fields = NULL,
      toJSON_args = list(auto_unbox = TRUE)
    ){
      self$set_transform_event(transform_event)
      self$set_transform_event_names(transform_event_names)
      self$set_excluded_fields(excluded_fields)
      self$set_toJSON_args(toJSON_args)
    },

    format_event = function(event) {
      values <- get(".transform_event", private)(event)

      if (is.character(self$transform_event_names)){
        original_names <- names(values)
        rename_idx <- match(original_names, names(self$transform_event_names), nomatch = 0L)
        names(values)[rename_idx > 0L] <- self$transform_event_names[rename_idx[rename_idx > 0L]]

      } else if (is.function(self$transform_event_names)){
        names(values) <- self$transform_event_names(names(values))

      } else {
        warning("`transform_event_names` must be a character vector or a function")
      }

      if (!is.null(self$excluded_fields)) {
        values <- values[!names(values) %in% self$excluded_fields]
      }

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
        "`transform_event` must be a function a single argument `event`"
      )

      private[[".transform_event"]] <- x
      invisible(self)
    },

    set_transform_event_names = function(x){
      assert(is.function(x) || is_field_name_map(x),
        "`transform_event_names` must be a named character vector or function with a single mandatory argument (optional arguments are OK)"
      )

      private[[".transform_event_names"]] <- x
    }
  ),


  active = list(
    #' @field toJSON_args a list of values passed on to [jsonlite::toJSON()]
    toJSON_args   = function() get(".toJSON_args", private),

    #' @field transform_event a `function` with a signle argument `event`.
    #' See [transform_event_dynatrace()] for an example.
    transform_event = function() get(".transform_event", private),

    #' @field transform_event_names a named `character` vector mapping original
    #'   field names to Dynatrace-compatible ones,
    #'   or a `function` that accepts a `character` vector of field names
    transform_event_names = function() get(".transform_event_names", private)
  ),


  private = list(
    .toJSON_args = NULL,
    .transform_event = NULL,
    .transform_event_names = NULL,
    .excluded_fields = NULL
  )
)


#' Transform a log event for Dynatrace
#'
#' Converts a [lgr::LogEvent] object into a list suitable for Dynatrace ingestion,
#' including optional field renaming and exclusion.
#'
#' @param event A [lgr::LogEvent] object.
#' @param excluded_fields A `character` vector of field names to exclude from
#'   the final output.
#' @param transform_event_names A named `character` vector mapping original
#'   field names to Dynatrace-compatible ones, or a function with a single
#'   mandatory argument that accepts a character vector of field names
#'
#' @returns A `list` of key-value pairs ready to be serialized to JSON for Dynatrace.
#' @seealso \url{https://docs.dynatrace.com/docs/discover-dynatrace/references/semantic-dictionary/fields#service}
#' @export
transform_event_dynatrace <- function(
    event
){
  values <- get("values", event)

  if (!is.null(values[["logger"]])) {
    values[["logger"]] <- gsub("/", ".", values[["logger"]], fixed = TRUE)
  }

  if (!is.null(values[["level"]])) {
    values[["level"]] <- unname(lgr::label_levels(values[["level"]]))
    values[["log.raw_level"]] <- values[["level"]]
  }

  values
}


is_field_name_map <- function(x){
  is.character(x) && !is.null(names(x)) && all(nzchar(names(x)))
}
