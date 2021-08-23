
parse_signal <- function(signal) {
  class(signal) <- c(class(signal), "covidcast_data_signal")

  #'
  #' fetch covidcast data
  #'
  #' @param data_source data source to fetch
  #' @param signals data source to fetch
  #' @param time_type data source to fetch
  #' @param time_values data source to fetch
  #' @param geo_type geo_type to fetch
  #' @param geo_values data source to fetch
  #' @param as_of data source to fetch
  #' @param issues data source to fetch
  #' @param lag data source to fetch
  #' @return an instance of epidata_call
  signal$call <- function(geo_type,
                          geo_values,
                          time_values,
                          ...) {
    covidcast(
      signal$source, signal$signal, signal$time_type, geo_type,
      time_values, geo_values, ...
    )
  }
  r <- list()
  r[[signal$signal]] <- signal
  r
}

parse_source <- function(source) {
  class(source) <- c(class(source), "covidcast_data_source")
  signals <- do.call(c, lapply(source$signals, parse_signal))
  source$signals <- signals
  signals_df <- as.data.frame(do.call(rbind, lapply(signals, function(x) {
    sub <- x[c(
      "source",
      "signal",
      "name",
      "active",
      "short_description",
      "description",
      "time_type",
      "time_label",
      "value_label",
      "format",
      "category",
      "high_values_are",
      "is_smoothed",
      "is_weighted",
      "is_cumulative",
      "has_stderr",
      "has_sample_size"
    )]
    sub$geo_types <- paste0(names(x$geo_types), collapse = ",")
    sub
  })))
  rownames(signals_df) <- paste(signals_df$source, signals_df$signal, sep = ":")
  source$signals_df <- signals_df
  r <- list()
  r[[source$source]] <- source
  r
}


#'
#' creates the covidcast epidata helper
#'
#' @param base_url optional alternative base url
#' @importFrom httr RETRY stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom rlang abort
#' @return TODO
#'
#' @export
covidcast_epidata <- function(base_url = global_base_url) {
  url <- join_url(base_url, "covidcast/meta")
  res <- do_request(url, list())

  httr::stop_for_status(res)
  r <- httr::content(res, "text", encoding = "UTF-8")
  meta <- jsonlite::fromJSON(r, simplifyVector = FALSE)

  sources <- do.call(c, lapply(meta, parse_source))
  sources_df <- as.data.frame(do.call(rbind, lapply(sources, function(x) {
    sub <- x[c(
      "source", "name", "description", "reference_signal", "license"
    )]
    sub$signals <- paste0(x$signals_df$signal, collapse = ",")
    sub
  })))
  rownames(sources_df) <- sources_df$source

  structure(
    list(
      base_url = base_url,
      sources = sources,
      sources_df = sources_df,
      signals_df = as.data.frame(do.call(rbind, lapply(sources, function(x) {
        x$signals_df
      })))
    ),
    class = "covidcast_epidata"
  )
}
