# AppenderDbi -------------------------------------------------------------


#' Log to databases via DBI
#'
#' Log to a database table with any **DBI** compatible backend. Please be
#' aware that AppenderDbi does *not* support case sensitive / quoted column
#' names, and you advised to only use all-lowercase names for
#' custom fields (see `...` argument of [LogEvent]).
#' When appending to a database table all LogEvent values for which a column
#' exists in the target table will be appended, all others are ignored.
#'
#' @section Buffered Logging:
#'
#' By default, AppenderDbi writes each LogEvent directly to the target database
#' which can be relatively slow. To improve performance it is possible to tell
#' AppenderDbi to buffer db writes by setting `buffer_size` to something greater
#' than `0`. This buffer is written to the database whenever it is full
#' (`buffer_size`), whenever a LogEvent with a level of `fatal` or `error` is
#' encountered (`flush_threshold`), or when the Appender is garbage collected
#' (`flush_on_exit`), i.e. when you close the \R session or shortly after you
#' remove the Appender object via `rm()`.
#'
#'
#' @section Creating a New Appender:
#'
#' An AppenderDbi is linked to a database table via its `table` argument. If the
#' table does not exist it is created either when the Appender is first
#' instantiated or (more likely) when the first LogEvent would be written to
#' that table. Rather than to rely on this feature, it is recommended that you
#' create the target table first using an `SQL CREATE TABLE` statement as this
#' is safer and more flexible. See also [LayoutDbi].
#'
#'
#' @section Choosing the correct DBI Layout:
#'
#' Layouts for relational database tables are tricky as they have very strict
#' column types and further restrictions. On top of that implementation details
#' vary between database backends.
#'
#' To make setting up `AppenderDbi` as painless as possible, the helper function
#' [select_dbi_layout()] tries to automatically determine sensible [LayoutDbi]
#' settings based on `conn` and - if it exists in the database already -
#' `table`. If `table` does not exist in the database and you start logging, a
#' new table will be created with the `col_types` from `layout`.
#'
#' @export
#' @family Appenders
#' @export
AppenderDbi <- R6::R6Class(
  "AppenderDbi",
  inherit = lgr::AppenderMemory,
  cloneable = FALSE,
  public = list(
    initialize = function(
      conn,
      table,
      threshold = NA_integer_,
      layout = select_dbi_layout(conn, table),
      close_on_exit = TRUE,
      buffer_size = 0,
      flush_threshold = "error",
      flush_on_exit = TRUE,
      flush_on_rotate = TRUE,
      should_flush = NULL,
      filters = NULL
    ){
      assert_namespace("DBI", "data.table", "jsonlite")

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

      if (!inherits(table, "Id")){
        table <- layout$format_table_name(table)
      }
      private$set_table(table)
      self$set_close_on_exit(close_on_exit)

      # table columns
      if (DBI::dbExistsTable(self$conn, self$table)){
        # do nothing
      } else if (is.null(self$layout$col_types)) {
        stop(AppenderConfigDoesNotMatchDbTableError(
          "table `%s` does not exist and no col_types were specified in `layout`"),
          self$table_name
        )

      } else {
        ct <- layout$col_types
        message(
          "creating '", fmt_tname(table), "' with columns: ",
          paste(names(ct), " ", ct, sep = "", collapse = ", ")
        )

        DBI::dbCreateTable(
          self$conn,
          self$table,
          fields = ct
        )
      }

      # table columns
      columns <- get_columns(self$conn, self$table)

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


    set_conn = function(conn){
      assert(inherits(conn, "DBIConnection"))

      if (inherits(conn, "MySQLConnection")){
        stop(
          "'RMySQL' is not supported by lgr. Please use the newer 'RMariaDB'",
          "package to connect to MySQL and MariaDB databases instead."
        )
      } else if (inherits(conn, "PostgreSQLConnection")){
        stop(
          "'PostgreSQL' is not supported by lgr. Please use the newer
          'Rpostgres' package to connect to Postgres databases instead."
        )
      }

      private$.conn <- conn
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

      if (identical(nrow(dd),  0L)){
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
      table  <- get("table", envir = self)
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

        DBI::dbWriteTable(
          conn  = get(".conn", envir = private),
          name  = table,
          value = dd,
          row.names = FALSE,
          append = TRUE
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
      fmt_tname(self$table)
    },


    #' @field conn a [DBI connection][DBI::dbConnect]
    conn = function(){
      private$.conn
    },


    #' @field close_on_exit `TRUE` or `FALSE`. Close the Database connection
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
        ct <- get_col_types(private[[".conn"]], self[["table"]])
        if (is.null(ct)) return (NULL)
        names(ct) <- get("layout", envir = self)[["format_colnames"]](names(ct))
        private$set_col_types(ct)
        return(ct)
      } else {
        get(".col_types", envir = private)
      }
    },


    #' @field table a `character` scalar or a [DBI::Id] specifying the target
    #'   database table
    table = function(){
      get(".table", envir = private)
    },


    #' @field table_name `character` scalar. Like `$table`, but always returns a
    #'   `character` scalar
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
    },


    data = function(){
      tbl <- get("table", envir = self)

      if (DBI::dbExistsTable(private[[".conn"]], tbl)){
        dd <- DBI::dbReadTable(private[[".conn"]], tbl)
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
        try(DBI::dbDisconnect(private$.conn), silent = TRUE)
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
    .conn = NULL,
    .table = NULL,
    .close_on_exit = NULL,
    .columns = NULL
  )
)




# AppenderRjdbc -------------------------------------------------------------

# exclude from coverage because relies on external resources
# nocov start

#' Log to databases via RJDBC
#'
#' Log to a database table with the **RJDBC** package. **RJDBC** is only
#' somewhat  **DBI** compliant and does not work with [AppenderDbi].
#' **I do not recommend using RJDBC if it can be avoided.**. AppenderRjdbc
#' is only tested for DB2 databases, and it is likely it will not work properly
#' for other databases. Please file a bug report if you encounter any issues.
#'
#' @export
#' @seealso [LayoutFormat], [simple_logging], [data.table::data.table]
#' @family Appenders
#' @export
AppenderRjdbc <- R6::R6Class(
  "AppenderRjdbc",
  inherit = AppenderDbi,
  cloneable = FALSE,
  public = list(
    initialize = function(
      conn,
      table,
      threshold = NA_integer_,
      layout = select_dbi_layout(conn, table),
      close_on_exit = TRUE,
      buffer_size = 0,
      flush_threshold = "error",
      flush_on_exit = TRUE,
      flush_on_rotate = TRUE,
      should_flush = NULL,
      filters = NULL
    ){
      assert_namespace("DBI", "RJDBC", "data.table")
      stop("AppenderRjdbc is currently not working")
      # TODO: AppenderRjdbc is disabled

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
      private$set_table(table)
      self$set_close_on_exit(close_on_exit)

      table_exists <- tryCatch(
        is.data.frame(DBI::dbGetQuery(conn, paste("SELECT 1 FROM", self$table))),
        error = function(e) FALSE
      )

      if (!table_exists) {
        message("Creating '", fmt_tname(self$table), "' with manually specified column types")
        RJDBC::dbSendUpdate(conn, layout$sql_create_table(self$table))
      }

      self
    },


    flush = function(){
      lo <- get(".layout", envir = private)

      table  <- get("table", envir = self)
      buffer <- get("buffer_dt", envir = self)

      if (length(buffer)){
        dd <- lo[["format_data"]](buffer)
        cn <- names(get("col_types", envir = self))

        if (!is.null(cn))
          dd <- dd[, intersect(cn, names(dd))]

        for (i in seq_len(nrow(dd))){
          data <- as.list(dd[i, ])
          q <-  sprintf(
            "INSERT INTO %s (%s) VALUES (%s)",
            get("table", self),
            paste(names(data), collapse = ", "),
            paste(rep("?", length(data)), collapse = ", ")
          )
          RJDBC::dbSendUpdate(get(".conn", private), q, list=data)
        }

        assign("insert_pos", 0L, envir = private)
        private$.buffer_events <- list()
        invisible(self)
      }
    }
  ),


  active = list(
    data = function(){
      dd <- try(DBI::dbGetQuery(self$conn, paste("SELECT * FROM", self$table)))

      if (inherits(dd, "try-error"))
        return(NULL)

      names(dd) <- tolower(names(dd))

      dd[["timestamp"]] <- as.POSIXct(dd[["timestamp"]])
      dd[["level"]] <- as.integer(dd[["level"]])
      dd
    }
  ),


  private = list(
    .conn = NULL,
    .table = NULL
  )
)




# helpers -----------------------------------------------------------------





fmt_tname <- function(x){
  if (inherits(x, "Id")){
    paste0("<Id: ", trimws(gsub("<Id>", "", toString(x))), ">")
  } else {
    x
  }
}




as_tname <- function(x){
  if (is_scalar_character(x)){
    return(x)

  } else if (inherits(x, "Id")){
    x <- x@name

    if (identical(length(x), 1L)){
      assert(identical(names(x), "table"))
      x <- x[["table"]]

    } else if (identical(length(x), 2L)){
      assert(setequal(names(x), c("schema", "table")))
      x <- paste0(x[["schema"]], ".", x[["table"]])

    } else {

      stop("Table identifiers must contain a table and may contain a schema")
    }
  }

  x
}




# embedded from tabde
#' Generate SQL CREATE TABLE statements
#'
#' Creates SQL CREATE TABLE statements from a vector of column names and
#' a vector of column types
#'
#' @param tname `character` scalar. Name of target sql table
#' @param col_names `character` vector. Column names of target sql table
#' @param col_types `character` scalar. Column types of target sql table.
#'   Columns of type `NA` will be skipped
#' @param sql_opts column options of target sql table (for example `NOT NULL`)
#'
#' @return a `CREATE TABLE` statement as a `character` scalar
#' @noRd
#'
#' @examples
#' sql_create_table(
#'   "example.table",
#'   c("numbers", "animals"),
#'   c("integer", "varchar(8)"),
#'   c("NOT NULL", "")
#' )
sql_create_table <- function(
  tname,
  col_names,
  col_types,
  sql_opts = rep("", length(col_names))
){
  # preconditions
  stopifnot(
    is_scalar_character(tname),
    is.character(col_names),
    is.character(col_types),
    is_equal_length(col_names, col_types, sql_opts)
  )

  assert(
    !anyNA(col_names) && all_are_distinct(col_names),
    "All `col_names` must be unique and non-`NA`"
  )

  sql_opts[is.na(sql_opts)] <- ""
  col_types  <- toupper(col_types)

  # process input
  if (anyNA(col_types)){
    message(sprintf(
      "Skipping %s columns where `col_type` equals `NA`", sum(is.na(col_types))
    ))
    col_names <- col_names[!is.na(col_types)]
    col_types <- col_types[!is.na(col_types)]
    sql_opts  <- sql_opts[!is.na(col_types)]
  }

  cols <- paste0(
    trimws(paste0(col_names, " ", col_types, " ", sql_opts)),
    collapse = ", "
  )

  sprintf("CREATE TABLE %s (%s)", tname, cols)
}





get_columns <- function(conn, table){

  res <- try(DBI::dbListFields(conn, table), silent = TRUE)

  if (!is_try_error(res))
    return(res)

  # hackish way if cannonical way above does not work
  res <- try(silent = TRUE, {
    if (is_try_error(res)){
      if (inherits(table, "Id"))
        table <- as_tname(table)

      dd  <- DBI::dbSendQuery(conn, paste("SELECT * FROM", table, "WHERE 1=0"))
      on.exit(DBI::dbClearResult(dd))
      cols <- DBI::dbColumnInfo(dd)

      if ("type" %in% names(cols)){
        cols$name
      } else if ("field.type" %in% names(cols)) {
        cols$field.name
      } else {
        stop("could not determine colnames")
      }
    }
  })

  if (!is_try_error(res))
    return(res)

  NULL
}



AppenderConfigDoesNotMatchDbTableError <- function(
  message,
  ...
){
  errorCondition(
    message = sprintf(message, ...),
    call = NULL,
    class = "AppenderConfigDoesNotMatchDbTableError"
  )
}




parse_timestamp_smart <- function(x){
  if (is.character(x) && !grepl("-", x[[1]]))
    x <- as.numeric(x)

  if (is.character(x)){
    as.POSIXct(x)
  } else {
    as.POSIXct(x, origin = c("1970-01-01 00:00:00"))
  }
}
