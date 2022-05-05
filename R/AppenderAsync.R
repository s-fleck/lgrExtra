AppenderAsync <- R6::R6Class(
  "AppenderAsync",
  inherit = lgr::Appender,
  public = list(

    initialize = function(
    appender
    ){
      self$set_appender(appender)
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
      private[[".future"]] <- future::future(private[[".appender"]][["append"]](event))
      private[[".future"]]
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

    #' @field file `character` scalar. path to the log file
    threshold = function() private[[".appender"]][["threshold"]],

    destination = function() private[[".appender"]][["destination"]],

    layout = function() private[[".appender"]][["layout"]],

    future = function() private[[".future"]],

    appender = function() private[[".appender"]],

    resolved = function() {
      future::resolved(private[[".future"]])
    }
  ),

  private = list(
    .appender = NULL,
    .future = NULL
  )
)
