
# LayoutDbi ---------------------------------------------------------------


#' Format log events for output to ElasticSearch
#'
#' Similar to [lgr::LayoutJson], but with some modifications to prepare data
#' for ElasticSearch.
#'
#' @template layout
#' @export
LayoutElasticSearch <- R6::R6Class(
  "LayoutElasticSearch",
  inherit = lgr::Layout,
  public = list(
    initialize = function(
      toJSON_args = list(auto_unbox = TRUE),
      transform_event = function(event) get("values", event)
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
