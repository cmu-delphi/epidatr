parse_signal <- function(signal, base_url) {
  class(signal) <- c("covidcast_data_signal", class(signal))
  signal$key <- paste(signal$source, signal$signal, sep = ":")

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
    epicall <- covidcast(
      signal$source, signal$signal, geo_type, signal$time_type,
      geo_values, time_values, ...
    )
    epicall$base_url <- base_url
    epicall
  }
  r <- list()
  r[[signal$signal]] <- signal
  r
}

#' @export
print.covidcast_data_signal <- function(signal, ...) {
  print(signal$name)
  print(signal$key)
  print(signal$short_description)
}

parse_source <- function(source, base_url) {
  class(source) <- c("covidcast_data_source", class(source))
  signals <- do.call(c, lapply(source$signals, parse_signal, base_url = base_url))
  class(signals) <- c("covidcast_data_signal_list", class(signals))
  source$signals <- signals
  r <- list()
  r[[source$source]] <- source
  r
}

#' @method as.data.frame covidcast_data_signal_list
#' @export
as.data.frame.covidcast_data_signal_list <- function(signals, ...) {
  as.data.frame(do.call(rbind, lapply(signals, function(x) {
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
  })), row.names = sapply(signals, function(x) {
    x$key
  }), ...)
}

#' @export
print.covidcast_data_source <- function(source, ...) {
  print(source$name, ...)
  print(source$source, ...)
  print(source$description, ...)
  signals <- as.data.frame(source$signals)
  print(signals[, c("signal", "name", "short_description")], ...)
}

#' creates the covidcast epidata helper
#'
#' Creates a helper object that can use auto-complete to help find covidcast
#' sources and signals.
#'
#' @examples
#' covidcast_api <- covidcast_epidata()
#' print(covidcast_api) # print available sources and signals
#' fb_survey <- covidcast_api$sources$`fb-survey` # tab completion for sources
#' print(fb_survey) # print a verbose list of available signals
#' smoothed_cli <- fb_survey$signals$smoothed_cli # tab completion for signals
#' print(smoothed_cli)
#' df <- smoothed_cli$call("nation", "us", epirange(20210405, 20210410))
#' @param base_url optional alternative API base url
#' @importFrom httr stop_for_status content http_type
#' @importFrom jsonlite fromJSON
#' @importFrom xml2 read_html xml_find_all xml_text
#' @return an instance of covidcast_epidata
#'
#' @export
covidcast_epidata <- function(base_url = global_base_url) {
  url <- join_url(base_url, "covidcast/meta")
  response <- do_request(url, list())

  if (response$status_code != 200) {
    # 500, 429, 401 are possible
    msg <- "fetch data from API"
    if (httr::http_type(response) == "text/html" & length(res$content) > 0) {
      # grab the error information out of the returned HTML document
      msg <- paste(msg, ":", xml2::xml_text(xml2::xml_find_all(
        xml2::read_html(content(response, "text")),
        "//p"
      )))
    }
    httr::stop_for_status(response, task = msg)
  }

  response_content <- httr::content(response, "text", encoding = "UTF-8")
  response_content <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)

  sources <- do.call(c, lapply(response_content, parse_source, base_url = base_url))
  class(sources) <- c("covidcast_data_source_list", class(sources))

  all_signals <- do.call(c, lapply(sources, function(x) {
    l <- c(x$signals)
    names(l) <- paste(x$source, names(l), sep = ":")
    l
  }))
  class(all_signals) <- c("covidcast_data_signal_list", class(all_signals))
  structure(
    list(
      sources = sources,
      signals = all_signals
    ),
    class = "covidcast_epidata"
  )
}

#' @method as.data.frame covidcast_data_source_list
#' @export
as.data.frame.covidcast_data_source_list <- function(sources, ...) {
  as.data.frame(do.call(rbind, lapply(sources, function(x) {
    sub <- x[c(
      "source", "name", "description", "reference_signal", "license"
    )]
    sub$signals <- paste0(sapply(x$signals, function(y) {
      y$signal
    }), collapse = ",")
    sub
  })), row.names = sapply(sources, function(x) {
    x$source
  }), ...)
}

#' @export
print.covidcast_epidata <- function(epidata, ...) {
  print("COVIDcast Epidata Fetcher")
  print("Sources:")
  sources <- as.data.frame(epidata$sources)
  print(sources[1:5, c("source", "name")], ...)
  if (nrow(sources) > 5) {
    print(paste0((nrow(sources) - 5), " more..."))
  }
  print("Signals")
  signals <- as.data.frame(epidata$signals)
  print(signals[1:5, c("source", "signal", "name")], ...)
  if (nrow(signals) > 5) {
    print(paste0((nrow(signals) - 5), " more..."))
  }
}
