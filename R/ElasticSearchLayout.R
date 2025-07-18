transform_event_elasticsearch = function(event) {
  dd <- event$values

  dd[["timestamp"]] <- as.numeric(dd[["timestamp"]]) * 1e3
  dd[["logger"]] <- gsub("/", ".", dd[["logger"]], fixed = TRUE)
  dd[["level"]] <- uppercase_first_letter(lgr::label_levels(dd[["level"]]))

  dd
}


#' A json layout for Elasticsearch ingestion
#'
#' @export
ElasticSearchLayout <- lgr::LayoutJson$new(
  transform_event = transform_event_elasticsearch,
  transform_event_names = c(
    "timestamp" = "@timestamp",
    "msg" = "message",
    "caller" = "callSite",
    "rawMsg" = "rawMessage"
  ),
  excluded_fields = NULL
)
