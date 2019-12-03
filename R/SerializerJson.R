Serializer <- R6::R6Class(
  "Serializer"
)



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
      cols_exclude = character(),
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
      jsonlite::toJSON(vals, auto_unbox = self$auto_unbox)
    }
  )
)
