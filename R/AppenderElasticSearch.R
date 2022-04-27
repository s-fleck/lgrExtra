#* @testfile integration_tests/test_AppenderElasticSearch
# AppenderDbi -------------------------------------------------------------


#' Log to elasticsearch
#'
#' **NOTE**: **Experimental**; not yet fully documented and and details are
#' subject to change
#'
#' @description
#'
#' @template appender
#'
#' @examples
#' @export
AppenderElasticSearch <- R6::R6Class(
  "AppenderElasticSearch",
  inherit = lgr::AppenderMemory,
  cloneable = FALSE,
  public = list(

    #' @param conn,index see section *Fields*
    #' @param threshold,flush_threshold,layout,buffer_size see [AppenderBuffer]
    initialize = function(
    conn,
    index,
    threshold = NA_integer_,
    layout = LayoutElasticSearch$new(),
    close_on_exit = TRUE,
    buffer_size = 0,
    flush_threshold = "error",
    flush_on_exit = TRUE,
    flush_on_rotate = TRUE,
    should_flush = NULL,
    filters = NULL
    ){
      assert_namespace("elastic", "data.table", "jsonlite")

      # appender
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)

      # buffer
      private$initialize_buffer(buffer_size)

      # flush conditions
      self$set_should_flush(should_flush)
      self$set_flush_threshold(flush_threshold)
      self$set_flush_on_exit(flush_on_exit)
      self$set_flush_on_rotate(flush_on_rotate)

      # database
      self$set_conn(conn)
      private$set_index(index)
      self$set_close_on_exit(close_on_exit)

      # index columns
      if (elastic::index_exists(self$conn, self$index)){
        # do nothing
      } else if (is.null(self$layout$col_types)) {
        elastic::index_create(conn, index = index)
      }

      self
    },


    set_close_on_exit = function(x){
      assert(is_scalar_bool(x))
      private$.close_on_exit <- x
      invisible(self)
    },


    set_conn = function(conn){
      assert(inherits(conn, "Elasticsearch"))

      tryCatch(
        conn$ping(),
        error = function(e) {
          stop(
            lg$fatal("Cannot connect to elasticsearch %s:%s: %s", conn$host, conn$port, e$message),
            e
          )
        }
      )

      private$.conn <- conn
      invisible(self)
    },


    #' @field data `data.frame`. content of index
    get_data = function(n = 20){
      index <- get("index", envir = self)

      if (elastic::index_exists(private[[".conn"]], index)){
        es_result <- elastic::Search(
          private[[".conn"]],
          index = index,
          body = '{
            "query": {
              "match_all": {}
            }
          }',
          size = 20
        )
      } else {
        return(NULL)
      }

      dd <- lapply(es_result$hits$hits, function(hit) wrap_recursive_elements(hit[["_source"]]))
      dd <- suppressWarnings(data.table::rbindlist(dd, use.names = TRUE, fill = TRUE))

      if (nrow(dd) > 0){
        if ("timestamp" %in% names(dd)){
          dd[["timestamp"]] <- parse_timestamp_smart(dd[["timestamp"]])
        } else if ("@timestamp" %in% names(dd)){
          dd[["@timestamp"]] <- parse_timestamp_smart(dd[["@timestamp"]])
        }
      }

      if (is_integerish(dd[["level"]])){
        dd[["level"]] <- as.integer(dd[["level"]])
      }

      dd
    },


    show = function(
    threshold = NA_integer_,
    n = 20
    ){
      stop("not implemented")
    },


    flush = function(){

      buffer <- get("buffer_events", envir = self)

      if (length(buffer)){
        # convert to data.frame (docs_bulk_index needs it that way)
          index  <- get("index", envir = self)

          # manually prepare data for bulk api so that we have more control
          # (esp. don't write NULL to empty fields but leave them out instead)
          # bulk API wants one line of metadata followed by the actual data
          json <- lapply(buffer, function(event) {
            json_header <- jsonlite::toJSON(list(index = list("_index" = index)), auto_unbox = TRUE)
            json_event  <- self[["layout"]][["format_event"]](event)

            c(
              json_header,
              json_event
            )
          })

          tf <- tempfile()
          on.exit(unlink(tf))
          writeLines(unlist(json), tf, useBytes = TRUE)  #useBytes = TRUE necessary for windows utf-8

        # insert into ES
          res <- suppressWarnings(elastic::docs_bulk(
            conn = self[["conn"]],
            x = tf,
            index = self[["index"]],
            quiet = TRUE
          ))

          if (isTRUE(res[["errors"]])){
            warning(jsonlite::toJSON(res))
          }

        # reset buffer
          assign("insert_pos", 0L, envir = private)
          private$.buffer_events <- list()

        invisible(self)
      }
    }
  ),


  # +- active ---------------------------------------------------------------
  active = list(
    destination = function(){
      self$index
    },


    #' @field conn a [ElasticSearch connection][elastic::connect]
    conn = function(){
      private$.conn
    },


    #' @field close_on_exit `TRUE` or `FALSE`. Close the ElasticSearch connection
    #'   when the Logger is removed?
    close_on_exit = function(){
      private$.close_on_exit
    },

    #' @field index a `character` scalar or a [DBI::Id] specifying the target
    #'   ElasticSearch index
    index = function(){
      get(".index", envir = private)
    }
  ),

  # +- private -------------------------------------------------------------
  private = list(
    finalize = function() {
      if (self$flush_on_exit)
        self$flush()

      if (self$close_on_exit){
        suppressWarnings(try(DBI::dbDisconnect(private$.conn), silent = TRUE))
      }
    },

    set_index = function(index){
      assert(is_scalar_character(index))
      private[[".index"]] <- index
      self
    },

    .conn = NULL,
    .index = NULL,
    .close_on_exit = NULL
  )
)




wrap_recursive_elements <- function(x){
  lapply(x,
    function(.){
      if (is.atomic(.)){
        .
      } else {
        list(.)
      }
    }
  )
}
