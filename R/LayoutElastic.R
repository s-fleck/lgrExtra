
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
      transform_names = NULL
    ){
      self$transform_names <- transform_names
      self
    },


    #' @field transform_names a `function` to transform the property names before
    #'   inserting to elasticsearch. For example, `LayoutElastic$new(transform_names = function(x) toupper(x))`
    transform_names   = NULL
  )
)


