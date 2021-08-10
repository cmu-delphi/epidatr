


create_epidata_call <- function(endpoint, params, meta = NULL) {
  stopifnot(is.character(endpoint), length(endpoint) == 1)
  stopifnot(is.list(params))
  stopifnot(is.null(meta) || is.list(meta))
  if(is.null(meta)) {
    meta <- list()
  }
  structure(
    list(
      endpoint = endpoint,
      params = params,
      base_url = BASE_URL,
      meta = meta
    ),
    class = "EpiDataCall"
  )
}

#'
#' use a different base url
#'
#' @param epidatacall and instance of EpiDataCall
#' @param base_url basee url to use
#'
#' @export
with_base_url <- function(epidatacall, base_url) {
  stopifnot(inherits(epidatacall, 'EpiDataCall'))
  stopifnot(is.character(base_url), length(base_url) == 1)
  epidatacall$base_url = base_url
  epidatacall
}

request_arguments <-
  function(epidatacall, format_type, fields = NULL) {
    stopifnot(inherits(epidatacall, 'EpiDataCall'))
    stopifnot(format_type %in% c('json', 'csv', 'classic'))
    stopifnot(is.null(fields) || is.character(fields))

    extra_params = list()
    if (format_type != 'classic') {
      extra_params[['format']] = format_type
    }
    if (!is.null(fields)) {
      extra_params[['fields']] = fields
    }
    all_params = c(epidatacall$params, extra_params)

    formatted_params = list()
    for (name in names(all_params)) {
      v = all_params[[name]]
      if (!is.null(v)) {
        formatted_params[[name]] = format_list(v)
      }
    }
    formatted_params
  }

full_url <- function(epidatacall) {
  stopifnot(inherits(epidatacall, 'EpiDataCall'))
  url = epidatacall$base_url
  if (url[length(url)] != '/') {
    url = paste0(url, '/')
  }
  paste0(url, epidatacall$endpoint)
}

#'
#' returns the full request url for the given epidatacall
#'
#' @param epidatacall and instance of EpiDataCall
#' @param format_type format to return one of classic,json,csv
#' @param fields filter fields
#' @importFrom httr modify_url
#' @return full url
#'
#' @export
request_url <- function(epidatacall, format_type = 'classic', fields = NULL) {
  stopifnot(inherits(epidatacall, 'EpiDataCall'))
  url = full_url(epidatacall)
  params = request_arguments(epidatacall, format_type, fields)
  httr::modify_url(url, query=params)
}

print.EpiDataCall <- function(epidatacall) {
  stopifnot(inherits(epidatacall, 'EpiDataCall'))
  print('EpiDataCall instance, use fetch_classic, fetch_json, fetch_df, fetch_csv to fetch the data')
  print(request_url(epidatacall))
}

request_impl <- function(epidatacall, format_type, fields = NULL) {
  stopifnot(inherits(epidatacall, 'EpiDataCall'))
  stopifnot(format_type %in% c('json', 'csv', 'classic'))
  # API call
  url = full_url(epidatacall)
  params = request_arguments(epidatacall, format_type, fields)

  res <- httr::GET(url, query = params, HTTP_HEADERS)
  if (res$status_code == 414) {
    res <- httr::POST(url, body = params, encode = 'form', HTTP_HEADERS)
  }
  res
}

#'
#' fetches the data and returns the classic format
#'
#' @param epidatacall and instance of EpiDataCall
#' @param fields filter fields
#' @importFrom httr GET POST stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom MMWRweek MMWRweek2Date
#' @return parsed json message
#'
#' @export
fetch_classic <- function(epidatacall, fields = NULL) {
  res <- request_impl(epidatacall, 'classic', fields)
  # TODO handle status messages as messages
  r <- httr::content(res, 'text', encoding='UTF-8')
  m <- jsonlite::fromJSON(r)
  if('epidata' %in% names(m)) {
      m$epidata = parse_data_frame(epidatacall, m$epidata)
  }
  m
}

#'
#' fetches the data and returns the josn format
#'
#' @param epidatacall and instance of EpiDataCall
#' @param fields filter fields
#' @importFrom httr GET POST stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom MMWRweek MMWRweek2Date
#' @return parsed json message
#'
#' @export
fetch_json <- function(epidatacall, fields = NULL) {
  res <- request_impl(epidatacall, 'json', fields)
  httr::stop_for_status(res)
  r <- httr::content(res, 'text', encoding='UTF-8')
  parse_data_frame(epidatacall, jsonlite::fromJSON(r))
}

#'
#' fetches the data and returns the CSV text
#'
#' @param epidatacall and instance of EpiDataCall
#' @param fields filter fields
#' @importFrom httr GET POST stop_for_status content
#' @return CSV text
#'
#' @export
fetch_csv <- function(epidatacall, fields = NULL) {
  res <- request_impl(epidatacall, 'csv', fields)
  httr::stop_for_status(res)
  httr::content(res, 'text', encoding='UTF-8')
}

#'
#' fetches the data and returns data frame
#'
#' @param epidatacall and instance of EpiDataCall
#' @param fields filter fields
#' @importFrom readr read_csv
#' @importFrom httr GET POST stop_for_status content
#' @importFrom MMWRweek MMWRweek2Date
#' @return data.frame
#'
#' @export
fetch_df <- function(epidatacall, fields = NULL) {
  r <- fetch_csv(epidatacall, fields)
  # TODO define column types
  parse_data_frame(epidatacall, readr::read_csv(r))
}
