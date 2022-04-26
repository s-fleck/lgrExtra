#* @testfile integration_tests/test_AppenderElastic
# AppenderDbi -------------------------------------------------------------


#' Log to elasticsearch
#'
#' @description
#'
#'
#' @section Buffered Logging:
#'
#' @section Creating a New Appender:
#'
#' @template appender
#'
#' @examples
#' @export
AppenderElastic <- R6::R6Class(
  "AppenderElastic",
  inherit = lgr::AppenderMemory,
  cloneable = FALSE,
  public = list(

    #' @param conn,index see section *Fields*
    #' @param threshold,flush_threshold,layout,buffer_size see [AppenderBuffer]
    initialize = function(
    conn,
    index,
    threshold = NA_integer_,
    layout = lgr::LayoutJson$new(),
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
        dd <- elastic::Search(
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

      dd <- lapply(dd$hits$hits, function(hit) as.data.frame(hit[["_source"]]))
      dd <- data.table::rbindlist(dd, use.names = TRUE, fill = TRUE)

      if (nrow(dd) > 0){
        dd[["timestamp"]] <- parse_timestamp_smart(dd[["timestamp"]])
      }

      dd[["level"]] <- as.integer(dd[["level"]])
      dd
    },


    show = function(
    threshold = NA_integer_,
    n = 20
    ){
      stop("not implemented")
    },


    flush = function(){
      lo <- get(".layout", envir = private)
      index  <- get("index", envir = self)
      buffer <- get("buffer_events", envir = self)

      if (length(buffer)){
        dd <- lapply(buffer, function(event) as.data.frame(event))
        dd <- data.table::rbindlist(dd, use.names = TRUE, fill = TRUE)
        dd <- as.data.frame(dd)

        elastic::docs_bulk_index(
          conn = self[["conn"]],
          x = dd,
          index = self[["index"]],
          quiet = TRUE
        )

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


    #' @field col_types a named `character` vector providing information about the
    #'   column types in the database. How the column types are reported
    #'   depends on the database driver. For example, SQLite returns human
    #'   readable data types (character, double, ...) while DB2 returns
    #'   numeric codes representing the data type
    #'   (see \url{https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_73/db2/rbafzcatsqltypeinfo.htm})
    col_types = function(){
      if (is.null(get(".col_types", envir = private))){
        ct <- get_col_types(private[[".conn"]], self[["index"]])
        if (is.null(ct)) return (NULL)
        names(ct) <- get("layout", envir = self)[["format_colnames"]](names(ct))
        private$set_col_types(ct)
        return(ct)
      } else {
        get(".col_types", envir = private)
      }
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


    set_col_types = function(x){
      if (!is.null(x)){
        assert(is.character(x))
        assert(identical(length(names(x)), length(x)))
      }
      private$.col_types <- x
      self
    },

    set_columns = function(x){
      assert(is.character(x))
      private$.columns <- x
      self
    },

    set_index = function(index){
      assert(is_scalar_character(index))
      private[[".index"]] <- index
      self
    },

    .col_types = NULL,
    .conn = NULL,
    .index = NULL,
    .close_on_exit = NULL,
    .columns = NULL
  )
)

