# combat with R < 3.6.0
errorCondition <- function(message, ..., class, call = NULL) {
  structure(
    class = c(class, "error", "condition"),
    list(message = as.character(message), call = call, ...)
  )
}



parse_timestamp_smart <- function(x){
  if (is.character(x) && !grepl("-", x[[1]])){
    x <- as.numeric(x)
  }

  if (is.character(x)){
    as.POSIXct(x)
  } else {
    # If x is > year 5000, we assume the data is milliseconds since 1970,
    # not seconds since 1970 what R expects
    if (max(x) > 95617580400){
      x <- x/1e3
    }

    as.POSIXct(x, origin = c("1970-01-01 00:00:00"))
  }
}
