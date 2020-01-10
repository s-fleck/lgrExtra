# combat with R < 3.6.0
errorCondition <- function(message, ..., class, call = NULL) {
  structure(
    class = c(class, "error", "condition"),
    list(message = as.character(message), call = call, ...)
  )
}
