
# LayoutDbi ---------------------------------------------------------------


#' Format Log Events for Output to Databases
#'
#' LayoutDbi can contain `col_types` that [AppenderDbi] can use to create new
#' database tables; however, it is safer and more flexible to set up the log
#' table up manually with an `SQL CREATE TABLE` statement instead.
#'
#' The LayoutDbi parameters `fmt`, `timestamp_fmt`, `colors` and `pad_levels`
#' are only applied for for console output via the `$show()` method and do not
#' influence database inserts in any way. The inserts are pre-processed by
#' the methods `$format_data()`, `$format_colnames` and `$format_tablenames`.
#'
#' It does not format
#' LogEvents directly, but their `data.table` representations (see
#' [as.data.table.LogEvent]), as well as column- and table names.
#'
#' @section Database Specific Layouts:
#'
#' Different databases have different data types and features. Currently the
#' following `LayoutDbi` subclasses exist that deal with specific databases,
#' but this list is expected to grow as lgr matures:
#'
#'   * `LayoutSqlite`: For SQLite databases
#'   * `LayoutPostgres`: for Postgres databases
#'   * `LayoutMySql`: for MySQL databases
#'   * `LayoutDb2`: for DB2 databases
#'
#' The utility function [select_dbi_layout()] tries returns the appropriate
#' Layout for a DBI connection, but this does not work for odbc and JDBC
#' connections where you have to specify the layout manually.
#'
#' For creating custom DB-specific layouts it should usually be enough to create
#' an [R6::R6] class that inherits from `LayoutDbi` and choosing different
#' defaults for `$format_table_name`, `$format_colnames` and `$format_data`.
#'
#'
#' @name LayoutDbi
#' @aliases LayoutSqlite LayoutRjdbc LayoutRjdbcDb2 LayoutDb2 LayoutMySql LayoutPostgres
#' @family Layouts
#' @family database layouts
#' @seealso [select_dbi_layout()], [DBI::DBI],
#'
#' @export
LayoutDbi <- R6::R6Class(
  "LayoutDbi",
  inherit = LayoutFormat,
  public = list(
    initialize = function(
      col_types = c(
        level = "integer",
        timestamp = "timestamp",
        logger = "varchar(256)",
        caller = "varchar(256)",
        msg = "varchar(2048)"
      ),
      serialized_cols = NULL,

      fmt = "%L [%t] %m  %f",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S",
      colors = getOption("lgr.colors", list()),
      pad_levels = "right",

      format_table_name = identity,
      format_colnames = identity,
      format_data = data.table::as.data.table
    ){
      self$set_col_types(col_types)
      self$set_serialized_cols(serialized_cols)

      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)

      self$format_table_name <- format_table_name
      self$format_colnames   <- format_colnames
      self$format_data       <- format_data

      self
    },


    #' @field format_table_name a `function` to format the table name before
    #'   inserting to the database. The function will be applied to the
    #'   `$table_name` before inserting into the database. For example some,
    #'   databases prefer all lowercase names, some uppercase. SQL updates
    #'   should be case-agnostic, but sadly in practice not all DBI backends
    #'   behave consistently in this regard.
    format_table_name = NULL,

    #' @field format_colnames a `function` to format the column names before
    #'   inserting to the database. The function will be applied to the column
    #'   names of the data frame to be inserted into the database.
    format_colnames   = NULL,

    #' @field format_data a `function` to format the data before
    #'   inserting into the database. The function will be applied to the whole
    #'   data frame.
    format_data       = NULL,


    set_col_types = function(x){
      if (!is.null(x)){
        assert(is.character(x))
        assert(identical(length(names(x)), length(x)))
      }
      private$.col_types <- x
      invisible(self)
    },


    set_serialized_cols = function(x){
      private$.serialized_cols <- x
      invisible(self)
    },

    sql_create_table = function(table){
      assert(
        !is.null(private$.col_types),
        "To create new database tables the Layout must contain `col_types`"
      )
      sql_create_table(
        tname = table,
        col_types = private$.col_types,
        col_names = names(private$.col_types)
      )
    },


    toString = function(){
      paste(
        fmt_class(class(self)[[1]]),
        paste(self$col_names, collapse = ", ")
      )
    }
  ),


  active = list(
    #' @field col_types a named `character` vector of column types supported by
    #'   the target database. If not `NULL` this is used by [AppenderDbi] or
    #'   similar Appenders to create a new database table on instantiation of
    #'   the Appender. If the target database table already exists, `col_types`
    #'   is not used.
    col_types = function() {
      r <- get(".col_types", envir = private)
      if (!is.null(r)){
        names(r) <- self$format_colnames(names(r))
      }
      r
    },


    #' @field names of the columns that contain data that has been serialized
    #'   to JSON strings
    serialized_cols = function(){
      get(".serialized_cols", envir = private)
    },


    #' @field col_names column names of the target table (the same as
    #'   `names(lo$col_types)`)
    col_names = function(){
      names(self$col_types)
    }
  ),


  private = list(
    .col_types = NULL,
    .serialized_cols   = NULL
  )
)




# +- LayoutSqlite ---------------------------------------------------------

#' @export
LayoutSqlite <- R6::R6Class(
  "LayoutSqlite",
  inherit = LayoutDbi,
  public = list(

    initialize = function(
      col_types = NULL,
      fmt = "%L [%t] %m  %f",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S",
      colors = getOption("lgr.colors", list()),
      pad_levels = "right",
      serialized_cols = NULL,

      format_table_name = tolower,
      format_colnames = tolower,
      format_data = function(x){
        x <- data.table::as.data.table(x)
        for (nm in names(x)){
          if (inherits(x[[nm]], "POSIXt"))
            data.table::set(x, i = NULL, j = nm, value = format(x[[nm]]))
        }
        x
      }
    ){
      self$set_col_types(col_types)
      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)

      self$set_serialized_cols(serialized_cols)

      self$format_table_name <- format_table_name
      self$format_colnames   <- format_colnames
      self$format_data       <- format_data

      self
    }
  )
)




# +- LayoutPostgres ----------------------------------------------------------

#' @export
LayoutPostgres <- R6::R6Class(
  "LayoutPostgres",
  inherit = LayoutDbi,
  public = list(
    initialize = function(
      col_types = NULL,
      fmt = "%L [%t] %m  %f",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S",
      colors = getOption("lgr.colors", list()),
      pad_levels = "right",

      format_table_name = function(x){
        if (inherits(x, "Id")) x else tolower(x)
      },
      format_colnames = tolower,
      format_data = identity
    ){
      self$set_col_types(col_types)
      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)

      self$format_table_name <- format_table_name
      self$format_colnames   <- format_colnames
      self$format_data       <- format_data

      self
    }
  )
)


# +- LayoutMySql ----------------------------------------------------------

#' @export
LayoutMySql <- R6::R6Class(
  "LayoutMySql",
  inherit = LayoutDbi,
  public = list(
    initialize = function(
      col_types = NULL,
      fmt = "%L [%t] %m  %f",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S",
      colors = getOption("lgr.colors", list()),
      pad_levels = "right",

      format_table_name = function(x) if (inherits(x, "Id")) x else tolower(x),
      format_colnames = tolower,
      format_data       = function(x){
        x <- data.table::as.data.table(x)
        data.table::setnames(x, tolower(names(x)))
        x
      }
    ){
      self$set_col_types(col_types)
      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)

      self$format_table_name <- format_table_name
      self$format_colnames   <- format_colnames
      self$format_data       <- format_data

      self
    }
  )
)





# +- LayoutDb2 ----------------------------------------------------------

#' @export
LayoutDb2 <- R6::R6Class(
  "LayoutDb2",
  inherit = LayoutDbi,
  public = list(
    initialize = function(
      col_types = NULL,
      fmt = "%L [%t] %m  %f",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S",
      colors = getOption("lgr.colors", list()),
      pad_levels = "right",
      serialized_cols = NULL,

      format_table_name = function(x){
        if (inherits(x, "Id")){
          x
        } else {
          toupper(x)
        }
      },
      format_colnames = toupper,
      format_data = function(x){
        x <- data.table::as.data.table(x)
        names(x) <- toupper(names(x))
        x
      }
    ){
      self$set_col_types(col_types)
      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)
      self$set_serialized_cols(serialized_cols)

      self$format_table_name <- format_table_name
      self$format_colnames   <- format_colnames
      self$format_data       <- format_data

      self
    }
  )
)


# +- LayoutRjdbc ----------------------------------------------------------


#' @export
LayoutRjdbc <- R6::R6Class(
  "LayoutDbi",
  inherit = LayoutDbi,
  public = list(
    initialize = function(
      col_types = NULL,
      fmt = "%L [%t] %m  %f",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S",
      colors = getOption("lgr.colors", list()),
      pad_levels = "right",

      format_table_name =  as_tname,
      format_colnames = toupper,
      format_data = function(x){
        x <- data.table::as.data.table(x)
        names(x) <- toupper(names(x))
        x
      }
    ){
      self$set_col_types(col_types)
      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)

      self$format_table_name <- format_table_name
      self$format_colnames   <- format_colnames
      self$format_data       <- format_data

      self
    }
  )
)




# +- LayoutRjdbcDb2 ----------------------------------------------------------

#' @export
LayoutRjdbcDb2 <- R6::R6Class(
  "LayoutDbi",
  inherit = LayoutDbi,
  public = list(
    initialize = function(
      col_types = NULL,
      fmt = "%L [%t] %m  %f",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S",
      colors = getOption("lgr.colors", list()),
      pad_levels = "right",

      format_table_name =  function(x) toupper(as_tname(x)),
      format_colnames = toupper,
      format_data = function(x){
        x <- data.table::as.data.table(x)
        names(x) <- toupper(names(x))
        x
      }
    ){
      self$set_col_types(col_types)
      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)

      self$format_table_name <- format_table_name
      self$format_colnames   <- format_colnames
      self$format_data       <- format_data

      self
    }
  )
)



# helpers -----------------------------------------------------------------



# utils -------------------------------------------------------------------


#' Select Appropriate Database Table Layout
#'
#' Selects an appropriate Layout for a database table based on
#' a DBI connection and - if it already exists in the database -
#' the table itself.
#'
#' @param conn  a [DBI connection][DBI::dbConnect()]
#' @param table a `character` scalar. The name of the table to log to.
#' @param ...  passed on to the appropriate `LayoutDbi` subclass constructor.
#'
#' @export
select_dbi_layout <- function(
  conn,
  table,
  ...
){
  cls <- c(class(conn))

  if (identical(class, "MySQLConnection")){
    stop(
      "'RMySQL' is not supported by lgr. Please use the newer 'RMariaDB'",
      "package to connect to MySQL and MariaDB databases"
    )
  }

  res <- switch(
    cls,
    "PostgreSQLConnection" = LayoutPostgres$new(...),

    "PqConnection" = LayoutPostgres$new(...),

    "MariaDBConnection" = LayoutMySql$new(...),

    "MySQLConnection" = LayoutMySql$new(...),

    "SQLiteConnection" = LayoutSqlite$new(
      col_types = c(
        level = "integer",
        timestamp = "TEXT",
        logger = "TEXT",
        caller = "TEXT",
        msg = "TEXT"
      ),
      ...
    ),

    "JDBCConnection" = LayoutRjdbc$new(
      col_types = c(
        level = "smallint",
        timestamp = "timestamp",
        logger = "varchar(256)",
        caller = "varchar(256)",
        msg = "varchar(2048)"
      ),
      ...
    ),
    LayoutDbi$new(...)
  )

  # check for DB2 on odbc connections
  try({
    if (grepl("^DB2", conn@info$dbms.name)){
      res$format_colnames   <- toupper
      res$format_table_name <- toupper
    }
  }, silent = TRUE)


  ct <- get_col_types(conn, table)

  if (!is.null(ct))  res$set_col_types(ct)

  res
}




get_col_types <- function(conn, table){
  res <- tryCatch({
    dd  <- DBI::dbSendQuery(conn, paste("SELECT * FROM", table))
    res <- DBI::dbColumnInfo(dd)
    DBI::dbClearResult(dd)
    if ("type" %in% names(res))
      setNames(as.character(res$type), tolower(res$name))
    else if ("field.type" %in% names(res))
      setNames(as.character(res$field.type), tolower(res$field.name))
  },
    error = function(e) NULL
  )

  res
}

