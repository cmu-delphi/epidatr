#' turn a signal into a callable
#' @param signal the signal of interest
#' @param base_url the base url
#' @keywords internal
parse_signal <- function(signal, base_url) {
  class(signal) <- c("covidcast_data_signal", class(signal))
  signal$key <- paste(signal$source, signal$signal, sep = ":")

  #' fetch covidcast data
  #'
  #' param data_source data source to fetch
  #' param signals data source to fetch
  #' param geo_type geo_type to fetch
  #' param time_type data source to fetch
  #' param geo_values data source to fetch
  #' param time_values data source to fetch
  #' param as_of data source to fetch
  #' param issues data source to fetch
  #' param lag data source to fetch
  #' return an instance of epidata_call
  #' keywords internal
  signal$call <- function(geo_type,
                          geo_values,
                          time_values,
                          as_of = NULL,
                          issues = NULL,
                          lag = NULL,
                          fetch_args = fetch_args_list()) {
    stopifnot(is.character(geo_type) & length(geo_type) == 1)

    pub_covidcast(
      source = signal$source,
      signals = signal$signal,
      geo_type = geo_type,
      time_type = signal$time_type,
      geo_values = geo_values,
      time_values = time_values,
      as_of = as_of,
      issues = issues,
      lag = lag,
      fetch_args = fetch_args
    )
  }
  r <- list()
  r[[signal$signal]] <- signal
  r
}

#' @export
print.covidcast_data_signal <- function(x, ...) {
  print(x$name)
  print(x$key)
  print(x$short_description)
}

parse_source <- function(source, base_url) {
  class(source) <- c("covidcast_data_source", class(source))
  signals <- do.call(c, unname(lapply(source$signals, parse_signal, base_url = base_url)))
  class(signals) <- c("covidcast_data_signal_list", class(signals))
  source$signals <- signals
  r <- list()
  r[[source$source]] <- source
  r
}

#' @method as_tibble covidcast_data_signal_list
#' @importFrom tibble as_tibble
#' @importFrom purrr map_chr map_lgl
#' @export
as_tibble.covidcast_data_signal_list <- function(x, ...) {
  tib <- list()
  chr_fields <- c(
    "source", "signal", "name", "short_description",
    "description", "time_type", "time_label", "value_label",
    "format", "category", "high_values_are"
  )
  for (field in chr_fields) {
    tib[[field]] <- unname(map_chr(x, field, .default = ""))
  }
  lgl_fields <- c("active")
  for (field in lgl_fields) {
    tib[[field]] <- unname(map_lgl(x, field, .default = ""))
  }
  as_tibble(tib)
}

#' @export
print.covidcast_data_signal_list <- function(x, ...) {
  tib <- as_tibble(x)
  print(tib[, c("source", "signal", "short_description")], ...)
}

#' @export
print.covidcast_data_source <- function(x, ...) {
  print(x$name, ...)
  print(x$source, ...)
  print(x$description, ...)
  signals <- as_tibble(x$signals)
  print(signals[, c("signal", "short_description")], ...)
}

#' Creates the COVIDcast Epidata autocomplete helper
#' @description
#' Creates a helper object that can use auto-complete to help find COVIDcast
#' sources and signals. The [COVIDcast
#' endpoint](https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html) of
#' the Epidata API contains many separate data sources and signals. It can be
#' difficult to find the name of the signal you're looking for, so you can use
#' `covidcast_epidata` to get help with finding sources and functions without
#' leaving R.
#'
#' The `covidcast_epidata()` function fetches a list of all signals, and returns
#' an object containing fields for every signal:
#' ```{r}
#' epidata <- covidcast_epidata()
#' epidata$signals
#' ```
#'
#' If you use an editor that supports tab completion, such as RStudio, type
#'   `epidata$signals$` and wait for the tab completion popup. You will be able
#'   to type the name of signals and have the autocomplete feature select them
#'   from the list for you. Note that some signal names have dashes in them, so
#'   to access them we rely on the backtick operator:
#'
#' ```{r}
#' epidata$signals$`fb-survey:smoothed_cli`
#' ```
#'
#' These objects can be used directly to fetch data, without requiring us to use
#' the `pub_covidcast()` function. Simply use the `$call` attribute of the object:
#'
#' ```{r}
#' epidata$signals$`fb-survey:smoothed_cli`$call("state", "pa",
#'                                               epirange(20210405, 20210410))
#' ```
#' @param base_url optional alternative API base url
#' @param timeout_seconds the maximum amount of time to wait for a response
#' @importFrom httr stop_for_status content http_type
#' @importFrom jsonlite fromJSON
#' @importFrom xml2 read_html xml_find_all xml_text
#' @return An instance of `covidcast_epidata`
#' @export
covidcast_epidata <- function(base_url = global_base_url, timeout_seconds = 30) {
  url <- join_url(base_url, "covidcast/meta")
  response <- do_request(url, list(), timeout_seconds)

  if (response$status_code != 200) {
    # 500, 429, 401 are possible
    msg <- "fetch data from API"
    if (httr::http_type(response) == "text/html" && length(response$content) > 0) {
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

  all_signals <- do.call(c, unname(
    lapply(sources, function(x) {
      l <- c(x$signals)
      names(l) <- paste(x$source, names(l), sep = ":")
      l
    })
  ))
  class(all_signals) <- c("covidcast_data_signal_list", class(all_signals))
  structure(
    list(
      sources = sources,
      signals = all_signals
    ),
    class = "covidcast_epidata"
  )
}

#' @method as_tibble covidcast_data_source_list
#' @export
as_tibble.covidcast_data_source_list <- function(x, ...) {
  tib <- list()
  fields <- c("source", "name", "description", "reference_signal", "license")
  for (field in fields) {
    tib[[field]] <- unname(map_chr(x, field, .default = ""))
  }
  as_tibble(tib)
}

#' @export
print.covidcast_epidata <- function(x, ...) {
  print("COVIDcast Epidata Fetcher")
  print("Sources:")
  sources <- as_tibble(x$sources)
  print(sources[, c("source", "name")], ...)

  print("Signals")
  signals <- as_tibble(x$signals)
  print(signals[, c("source", "signal", "name")], ...)
}
