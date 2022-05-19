#' Log to Async via futures
#'
#' @description
#' This Appender can be used as wrapper around another Appender to make it
#' async (i.e. not block until the log message is written to the destination
#' but rather return imediatly). This makes most sense for "slow" destinations
#' like Databases or ElasticSearch.
#'
#' **NOTE**: **Experimental**; not yet fully documented and and details are
#' subject to change
#'
#' @template appender
#'
#' @examples
#' \dontrun{
#'   app <- AppenderAsync$new(AppenderFile$new(file = tempfile()))
#'   lg <- get_logger("test/async")$add_appender(app)
#'
#'   # you also have to configure the future::plan to actually enable async
#'   # logging
#'   future::plan("multisession")
#'
#'   lg$info("This will be written to a file asynchronously!")
#' }
#'
#' @export
AppenderAsync <- R6::R6Class(
  "AppenderAsync",
  inherit = lgr::Appender,
  public = list(

    #' @description
    #' @param appender a single `Appender` that will be called asynchronously
    initialize = function(
      appender
    ){
      self$set_appender(appender)
      private[[".futures"]] <- list()
      self
    },


    set_appender = function(appender){
      private[[".appender"]] <- appender
    },

    set_threshold = function(level){
      private[[".appender"]][["set_threshold"]](level)
    },

    set_layout = function(layout){
      private[[".appender"]][["set_layout"]](layout)
    },

    append = function(event){
      self$prune_futures()
      len <- length(private[[".futures"]])
      private[[".futures"]][[len + 1L]] <- future::future(private[[".appender"]][["append"]](event))
      private[[".futures"]]
    },

    prune_futures = function(){
      for (i in rev(seq_along(private[[".futures"]]))){
        if (identical(future::resolved(private[[".futures"]][[i]]), TRUE)){
          private[[".futures"]][[i]] <- NULL
        }
      }
    },

    await = function(){
      future::resolve(private[[".futures"]])
      self
    },

    format = function(
    ...
    ){
      async_appender_header <- paste0("<", class(self)[[1]], " : ")
      res <- self$appender$format()
      res <- substring(res, 2)

      res[[1]] <- paste0(async_appender_header, res[[1]])
      res
    }

  ),

  # +- active ---------------------------------------------------------------
  active = list(

    #' @field threshold the threshold of the wrapped Appender
    threshold = function() private[[".appender"]][["threshold"]],

    #' @field destination the destination of the wrapped Appender (with the
    #' Appender's class prefixed)
    destination = function() paste0(fmt_class(class(private[[".appender"]])[[1]]), " -> ", private[[".appender"]][["destination"]]),

    #' @field layout the layout of the wrapped Appender
    layout = function() private[[".appender"]][["layout"]],

    #' @field futures a list of futures, one for each ongoing `$append` operation
    futures = function() private[[".futures"]],

    #' @field appender the wrapped Appender
    appender = function() private[[".appender"]],

    #' @field layout `logical` scalar. Whether or not all futures have resolved
    #'  (i.e. no log message is outstanding)
    resolved = function() {
      for (el in private[[".futures"]]){
        if (identical(future::resolved(el), FALSE)){
          return (FALSE)
        }
      }
      TRUE
    }
  ),

  private = list(
    .appender = NULL,
    .futures = NULL
  )
)

