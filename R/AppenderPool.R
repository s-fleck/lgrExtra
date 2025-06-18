# AppenderPool -------------------------------------------------------------

#' Log to databases via pool
#'
#' @description
#'
#' Log to a database table using a connection pool from the **pool** package.
#' This provides better performance and connection management compared to
#' direct DBI connections, especially for applications with concurrent users.
#' Like AppenderDbi, it does *not* support case sensitive / quoted column
#' names, and you are advised to only use all-lowercase names for
#' custom fields (see `...` argument of [lgr::LogEvent]).
#'
#' @section Benefits of Pooled Connections:
#'
#' Using connection pools instead of direct DBI connections provides several advantages:
#' - Connections are reused rather than created for each query
#' - Connection management is automated (creation, validation, destruction)
#' - Better handles concurrent requests in multi-user applications
#' - Improves overall performance by reducing connection overhead
#'
#' @section Buffered Logging:
#'
#' Like AppenderDbi, AppenderPool supports buffered logging by setting `buffer_size`
#' to something greater than `0`. This buffer is written to the database whenever it is full
#' (`buffer_size`), whenever a LogEvent with a level of `fatal` or `error` is
#' encountered (`flush_threshold`), or when the Appender is garbage collected
#' (`flush_on_exit`).
#'
#' @section Creating a New Appender:
#'
#' An AppenderPool is linked to a database table via its `table` argument. If the
#' table does not exist it is created either when the Appender is first
#' instantiated or when the first LogEvent would be written to that table.
#' It is recommended to create the target table first using an `SQL CREATE TABLE`
#' statement for more control and safety.
#'
#' @template appender
#'
#' @examples
#' if (requireNamespace("RSQLite") && requireNamespace("pool")){
#'   pool <- pool::dbPool(
#'     drv = RSQLite::SQLite(),
#'     dbname = ":memory:"
#'   )
#'
#'   app <- AppenderPool$new(
#'     pool = pool,
#'     table = "log"
#'   )
#'
#'   lg <- lgr::get_logger("test/pool")$
#'     add_appender(app, "db")$
#'     set_propagate(FALSE)
#'   lg$info("test")
#'   print(lg$appenders[[1]]$data)
#'
#'   invisible(lg$config(NULL)) # cleanup
#'   pool::poolClose(pool)
#' }
#' @export
AppenderPool <- R6::R6Class(
  "AppenderPool",
  inherit = lgr::AppenderMemory,
  cloneable = FALSE,

  public = list(
    #' @param pool,table see section *Fields*
    #' @param threshold,flush_threshold,layout,buffer_size see [lgr::AppenderBuffer]
    initialize = function(
      pool,
      table,
      threshold = NA_integer_,
      layout = select_dbi_layout(pool::poolCheckout(pool), table),
      close_on_exit = FALSE,  # Changed default to FALSE since pool manages connections
      buffer_size = 0,
      flush_threshold = "error",
      flush_on_exit = TRUE,
      flush_on_rotate = TRUE,
      should_flush = NULL,
      filters = NULL
    ){
      assert_namespace("DBI", "data.table", "jsonlite", "pool")

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
      self$set_pool(pool)

      if (!inherits(table, "Id")){
        table <- layout$format_table_name(table)
      }

      private$set_table(table)
      self$set_close_on_exit(close_on_exit)

      # Check if table exists and create if needed
      conn <- pool::poolCheckout(pool)
      on.exit(pool::poolReturn(conn))

      if (DBI::dbExistsTable(conn, self$table)){
        # do nothing
      } else if (is.null(self$layout$col_types)) {
        stop(AppenderConfigDoesNotMatchDbTableError(
          "table `%s` does not exist and no col_types were specified in `layout`",
          self$table_name
        ))
      } else {
        ct <- layout$col_types
        message(
          "creating '", fmt_tname(table), "' with columns: ",
          paste(names(ct), " ", ct, sep = "", collapse = ", ")
        )

        DBI::dbCreateTable(
          conn,
          self$table,
          fields = ct
        )
      }

      # table columns
      columns <- get_columns(conn, self$table)
      if (is.null(columns)){
        stop(AppenderConfigDoesNotMatchDbTableError(
          "Could not retrieve column names of `%s`. Please supply them manually",
          self$table_name
        ))
      }

      private$set_columns(columns)
      assert(
        all(
          layout$format_colnames(names(layout$serialized_cols)) %in%
            columns
        ),
        AppenderConfigDoesNotMatchDbTableError(
          "The following `serialized_cols` were defined but are not present in %s: %s",
          self$table_name,
          paste(setdiff(names(layout$serialized_cols), columns), collapse = ", ")
        )
      )

      self
    },

    set_close_on_exit = function(x){
      assert(is_scalar_bool(x))
      private$.close_on_exit <- x
      invisible(self)
    },

    set_pool = function(pool){
      assert(inherits(pool, "Pool"))
      private$.pool <- pool
      invisible(self)
    },

    show = function(
      threshold = NA_integer_,
      n = 20
    ){
      assert(is_n0(n))
      threshold <- standardize_threshold(threshold)
      if (is.na(threshold)) threshold <- Inf
      dd <- tail(self$data[self$data$level <= threshold, ], n)
      colors <- getOption("lgr.colors")
      lo <- get(".layout", envir = private)
      if (identical(nrow(dd), 0L)){
        cat("[empty log]")
      } else {
        walk(
          as_event_list(dd, na.rm = TRUE),
          function(.x){
            cat(lo$format_event(.x), "\n", sep = "")
          }
        )
      }
      invisible(dd)
    },

    flush = function(){
      lo <- get(".layout", envir = private)
      table <- get("table", envir = self)
      buffer <- get("buffer_events", envir = self)

      if (length(buffer)){
        dd <- lo[["format_data"]](buffer)
        cn <- get(".columns", envir = private)
        sc <- lapply(
          lo$serialized_cols,
          function(.s) vapply(buffer, function(.) .s$serialize(.), character(1), USE.NAMES = FALSE)
        )

        dd <- data.table::as.data.table(dd)
        sel <- which(toupper(names(dd)) %in% toupper(cn))
        dd <- dd[, sel, with = FALSE]

        # add normal cols
        for (nm in names(which(vapply(dd, Negate(is.atomic), logical(1))))){
          data.table::set(
            dd,
            i = NULL,
            j = nm,
            value = vapply(
              dd[[nm]],
              function(.) if (is.null(.)) NA_character_ else jsonlite::toJSON(., auto_unbox = TRUE),
              character(1L)
            )
          )
        }

        # add serialized cols
        for (nm in names(sc)){
          assert(is.character(sc[[nm]]))
          data.table::set(
            dd,
            i = NULL,
            j = nm,
            value = as.character(sc[[nm]])
          )
        }

        data.table::setnames(dd, lo[["format_colnames"]](names(dd)))

        # Use the pool to write to the database
        pool::poolWithTransaction(private$.pool, function(conn) {
          DBI::dbWriteTable(
            conn = conn,
            name = table,
            value = dd,
            row.names = FALSE,
            append = TRUE
          )
        })

        assign("insert_pos", 0L, envir = private)
        private$.buffer_events <- list()
      }

      invisible(self)
    }
  ),

  # +- active ---------------------------------------------------------------
  active = list(
    destination = function(){
      fmt_tname(self$table)
    },

    #' @field pool a [pool connection][pool::dbPool]
    pool = function(){
      private$.pool
    },

    #' @field close_on_exit `TRUE` or `FALSE`. Close the pool connection
    #' when the Logger is removed? Usually not necessary as pools manage their own lifecycle.
    close_on_exit = function(){
      private$.close_on_exit
    },

    #' @field col_types a named `character` vector providing information about the
    #' column types in the database.
    col_types = function(){
      if (is.null(get(".col_types", envir = private))){
        conn <- pool::poolCheckout(private[[".pool"]])
        on.exit(pool::poolReturn(conn))

        ct <- get_col_types(conn, self[["table"]])
        if (is.null(ct)) return (NULL)
        names(ct) <- get("layout", envir = self)[["format_colnames"]](names(ct))
        private$set_col_types(ct)
        return(ct)
      } else {
        get(".col_types", envir = private)
      }
    },

    #' @field table a `character` scalar or a [DBI::Id] specifying the target
    #' database table
    table = function(){
      get(".table", envir = private)
    },

    #' @field table_name `character` scalar. Like `$table`, but always returns a
    #' `character` scalar
    table_name = function(){
      as_tname(get("table", envir = self))
    },

    #' @field table_id `DBI::Id`. Like `$table`, but always returns a [DBI::Id]
    table_id = function(){
      table <- self$table
      table <- unlist(strsplit(table, ".", fixed = TRUE))
      if (identical(length(table), 1L)){
        table <- DBI::Id(table = table)
      } else if (identical(length(table), 2L)) {
        table <- DBI::Id(schema = table[[1]], table = table[[2]])
      } else {
        stop(
          "`table` must either be DBI::Id object or a character scalar of ",
          "the form <schema>.<table>")
      }
      table
    },

    data = function(){
      tbl <- get("table", envir = self)
      conn <- pool::poolCheckout(private[[".pool"]])
      on.exit(pool::poolReturn(conn))

      if (DBI::dbExistsTable(conn, tbl)){
        dd <- DBI::dbReadTable(conn, tbl)
      } else {
        return(NULL)
      }

      names(dd) <- tolower(names(dd))
      if (nrow(dd) > 0){
        dd[["timestamp"]] <- parse_timestamp_smart(dd[["timestamp"]])
      }

      dd[["level"]] <- as.integer(dd[["level"]])
      dd
    },

    dt = function(){
      data.table::as.data.table(self$data)
    }
  ),

  # +- private -------------------------------------------------------------
  private = list(
    finalize = function() {
      if (self$flush_on_exit)
        self$flush()

      if (self$close_on_exit){
        # Only attempt to close if specified (usually not needed with pool)
        suppressWarnings(try(pool::poolClose(private$.pool), silent = TRUE))
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

    set_table = function(table){
      if (inherits(table, "Id")){
        assert("table" %in% names(table@name))
      } else {
        assert(is_scalar_character(table))
      }
      private[[".table"]] <- table
      self
    },

    .col_types = NULL,
    .pool = NULL,
    .table = NULL,
    .close_on_exit = NULL,
    .columns = NULL
  )
)
