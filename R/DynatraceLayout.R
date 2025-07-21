
#' Transform a log event for Dynatrace
#'
#' Converts a [lgr::LogEvent] object into a list suitable for Dynatrace ingestion,
#' including optional field renaming and exclusion.
#'
#' @param event A [lgr::LogEvent] object.
#' @param excluded_fields A `character` vector of field names to exclude from
#'   the final output.
#' @param transform_event_names A named `character` vector mapping original
#'   field names to Dynatrace-compatible ones, or a function with a single
#'   mandatory argument that accepts a character vector of field names
#'
#' @returns A `list` of key-value pairs ready to be serialized to JSON for Dynatrace.
#' @seealso \url{https://docs.dynatrace.com/docs/discover-dynatrace/references/semantic-dictionary/fields#service}
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


#' A json layout for Dynatrace ingestion
#'
#' @export
DynatraceLayout <- lgr::LayoutJson$new(
  transform_event = transform_event_dynatrace,
  transform_event_names = c(
    "msg" = "content",
    "level" = "loglevel",
    "logger" = "log.logger",
    "caller" = "code.function",
    "rawMsg" = "log.record.template"  # inspired by https://github.com/open-telemetry/semantic-conventions/issues/2064
  ),
  excluded_fields = NULL
)
