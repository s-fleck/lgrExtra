#' @param event A [lgr::LogEvent] object.
#'
#' @returns A `list` of key-value pairs ready to be serialized to JSON for ElasticSearch
#' @rdname ElasticSearchLayout
#' @export
transform_event_elasticsearch = function(event) {
  dd <- event$values

  dd[["timestamp"]] <- as.numeric(dd[["timestamp"]]) * 1e3
  dd[["logger"]] <- gsub("/", ".", dd[["logger"]], fixed = TRUE)
  dd[["level"]] <- lgr::label_levels(dd[["level"]])

  dd
}


#' A json layout for Elasticsearch ingestion
#'
#' Transforms a [lgr::LogEvent] object into a list suitable for ElasticSearch ingestion.
#'
#' @seealso https://www.elastic.co/docs/reference/ecs
#' @export
ElasticSearchLayout <- lgr::LayoutJson$new(
  transform_event = transform_event_elasticsearch,
  transform_event_names = c(
    "@timestamp" = "timestamp",
    "message" = "msg",
    "log.origin.function" = "caller",
    "log.record.template" = "rawMsg",
    "log.level" = "level",
    "log.logger" = "logger"
  ),
  excluded_fields = NULL
)
