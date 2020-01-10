#' Serializers
#'
#' @export
Serializer <- R6::R6Class(
  "Serializer"
)



#' @export
#' @rdname Serializer
SerializerJson <- R6::R6Class(
  "SerializerJson",
  inherit = Serializer,
  public = list(
    cols = NULL,
    cols_exclude = NULL,
    col_filter = NULL,
    max_nchar = NULL,
    auto_unbox = NULL,

    initialize = function(
      cols = "*",
      cols_exclude = c("level", "timestamp", "logger", "caller", "msg"),
      col_filter = is.atomic,
      max_nchar = 2048L,
      auto_unbox = TRUE
    ){
      self$cols <- cols
      self$cols_exclude <- cols_exclude
      self$col_filter <- col_filter
      self$max_nchar <- nchar
      self$auto_unbox <- auto_unbox
    },

    serialize = function(event){
      vals <- event$values
      vals <- vals[!names(vals) %in% self$cols_exclude]
      vals <- vals[vapply(vals, self$col_filter, logical(1), USE.NAMES = FALSE)]

      if (length(vals)){
        jsonlite::toJSON(vals, auto_unbox = self$auto_unbox)
      } else {
        NA_character_
      }
    }
  )
)




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
unpack_json_cols <- function(
  x,
  cols
){
  UseMethod("unpack_json_cols")
}




#' Title
#'
#' @param x
#' @param cols
#'
#' @return
#' @export
#'
#' @examples
unpack_json_cols.data.table <- function(
  x,
  cols
){
  assert_namespace("jsonlite")

  a <- list(x[ , !cols, with = FALSE])
  b <- lapply(cols, function(nm) unpack_col(x[[nm]]))


  do.call(cbind, c(a, b))
}



#' Title
#'
#' @param x
#' @param cols
#'
#' @return
#' @export
#'
#' @examples
unpack_json_cols.data.frame <- function(
  x,
  cols
){
  as.data.frame(
    unpack_json_cols.data.table(
      data.table::as.data.table(x),
      cols = cols
    )
  )
}





unpack_row <- function(x){
  if (is.na(x)){
    data.table(..unpack_row_dummy.. = list(NULL))
  } else if (identical(x, "NA")){
    warning("row contains 'NA' string value")
    data.table(..unpack_row_dummy.. = list(NULL))
  } else {
    data.table::as.data.table(
      lapply(jsonlite::fromJSON(x), function(.) if (is_scalar_atomic(.)) . else list(.))
    )
  }
}




unpack_col <- function(x){
  r <- lapply(x, unpack_row)
  r <- data.table::rbindlist(r, fill = TRUE, use.names = TRUE)
  if ("..unpack_row_dummy.." %in% names(r)){
    r[, ..unpack_row_dummy.. := NULL]
  }

  r
}
