
# LayoutDbi ---------------------------------------------------------------


#' Format log events for output to ElasticSearch
#'
#' @template layout
#' @family database layouts
#' @export
LayoutElastic <- R6::R6Class(
  "LayoutElastic",
  inherit = lgr::Layout,
  public = list(
    initialize = function(
      transform_names = NULL,
      transform_data = data.table::as.data.table
    ){
      self$transform_names <- transform_names
      self$transform_data <- transform_data
      self
    },


    #' @field transform_names a `function` to transform the property names before
    #'   inserting to elasticsearch. For example, `LayoutElastic$new(transform_names = function(x) toupper(x))`
    transform_names = NULL,

    #' @field transform_data a `function` to transform the log event to
    #' a `data.frame` before inserting to ElasticSearch. Must return a
    #' `data.frame` or `data.table` that will be passed to
    #' `data.table::rbindlist()`.
    transform_data = NULL
  )
)


