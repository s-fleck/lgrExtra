#' @param event A [lgr::LogEvent] object.
#'
#' @returns A `list` of key-value pairs ready to be serialized to JSON for Dynatrace.
#' @rdname DynatraceLayout
#' @export
transform_event_dynatrace <- function(
    event
){
  values <- get("values", event)

  if (!is.null(values[["logger"]])) {
    values[["logger"]] <- gsub("/", ".", values[["logger"]], fixed = TRUE)
  }

  if (!is.null(values[["level"]])) {
    values[["level"]] <- unname(lgr::label_levels(values[["level"]]))
    values[["log.raw_level"]] <- values[["level"]]
  }

  values
}


#' Convert POSIXct to Unix Epoch milliseconds
#'
#' Useful for log targets that expect timestamps as unix epoch milliseconds,
#' such as Dynatrace and Elasticsearch.
#'
#' @param x a `POSIXct` vector
#' @param digits an `integer` scalar. Number of decimal places to round to.
#'
#' @export
as_unix_epoch_ms <- function(x, digits = 0){
  round(as.numeric(x) * 1e3, digits = digits)
}


#' A json layout for Dynatrace ingestion
#'
#' Transforms a [lgr::LogEvent] object into a list suitable for Dynatrace ingestion.
#'
#' @seealso \url{https://docs.dynatrace.com/docs/discover-dynatrace/references/semantic-dictionary/fields#service}
#' @export
DynatraceLayout <- lgr::LayoutJson$new(
  transform_event = transform_event_dynatrace,
  timestamp_fmt = as_unix_epoch_ms,
  transform_event_names = c(
    "content" = "msg",
    "loglevel" = "level",
    "log.logger" = "logger",
    "code.function" = "caller",
    "log.record.template" = "rawMsg"   # inspired by https://github.com/open-telemetry/semantic-conventions/issues/2064
  ),
  excluded_fields = NULL
)
