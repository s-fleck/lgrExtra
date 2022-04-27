
# LayoutDbi ---------------------------------------------------------------


#' Format log events for output to ElasticSearch
#'
#' @template layout
#' @family database layouts
#' @export
LayoutElasticSearch <- R6::R6Class(
  "LayoutElasticSearch",
  inherit = lgr::Layout,
  public = list(
    initialize = function(
      transform_names = NULL,
      transform_data = function(event) event[["values"]]
    ){
      self$transform_data <- transform_data
      self
    },

    #' @field transform_data a `function` to transform the log event to a list.
    transform_data = NULL
  )
)


