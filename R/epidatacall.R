


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

  # don't retry in case of certain status codes
  res <- httr::RETRY('GET', url, query = params, HTTP_HEADERS, terminate_on=c(400, 401, 403, 405, 414, 500))
  if (res$status_code == 414) {
    res <- httr::RETRY('POST', url, body = params, encode = 'form', HTTP_HEADERS, terminate_on=c(400, 401, 403, 405, 414, 500))
  }
  res
}

#'
#' fetches the data and returns the classic format
#'
#' @param epidatacall and instance of EpiDataCall
#' @param fields filter fields
#' @importFrom httr RETRY stop_for_status content http_error
#' @importFrom jsonlite fromJSON
#' @importFrom MMWRweek MMWRweek2Date
#' @return parsed json message
#'
#' @export
fetch_classic <- function(epidatacall, fields = NULL, disable_date_parsing = FALSE) {
  res <- request_impl(epidatacall, 'classic', fields)
  r <- httr::content(res, 'text', encoding='UTF-8')
  if (httr::http_error(res)) {
    # return message in case of error
    return(list(result=0, message=paste0("error: ", r), epidata=data.frame()))
  }

  m <- jsonlite::fromJSON(r)
  if('epidata' %in% names(m)) {
      m$epidata = parse_data_frame(epidatacall, m$epidata, disable_date_parsing = disable_date_parsing)
  }
  m
}

#'
#' fetches the data and returns the josn format
#'
#' @param epidatacall and instance of EpiDataCall
#' @param fields filter fields
#' @importFrom httr RETRY stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom MMWRweek MMWRweek2Date
#' @return parsed json message
#'
#' @export
fetch_json <- function(epidatacall, fields = NULL, disable_date_parsing = FALSE) {
  res <- request_impl(epidatacall, 'json', fields)
  httr::stop_for_status(res)
  r <- httr::content(res, 'text', encoding='UTF-8')
  parse_data_frame(epidatacall, jsonlite::fromJSON(r), disable_date_parsing = disable_date_parsing)
}

#'
#' fetches the data and returns the CSV text
#'
#' @param epidatacall and instance of EpiDataCall
#' @param fields filter fields
#' @importFrom httr RETRY stop_for_status content
#' @return CSV text
#'
#' @export
fetch_csv <- function(epidatacall, fields = NULL) {
  res <- request_impl(epidatacall, 'csv', fields)
  httr::stop_for_status(res)
  data <- httr::content(res, 'text', encoding='UTF-8')
  class(data) <- c("EpiDataCSV", class(data))
  data
}

print.EpiDataCSV <- function(x, ...) {
  char.limit = getOption("csv__char_limit", default = 300L)
  cat('# A EpiDataCSV object with', nchar(x), 'characters; showing up to', char.limit, 'characters below. To print the entire string, use `print(as.character(x))`:\n')
  cat(substr(x, 1L, char.limit))
  if (nchar(x) > char.limit) {
    cat("[...]")
  }
  cat("\n")
  invisible(x)
}

info_to_type <- function(info, disable_date_parsing = FALSE) {
  types = list(
    date=if (disable_date_parsing) { readr::col_integer() } else { readr::col_date(format="%Y%m%d") },
    epiweek=readr::col_integer(),
    bool=readr::col_logical(),
    text=readr::col_character(),
    int=readr::col_integer(),
    float=readr::col_double(),
    categorical=readr::col_factor(info$categories, TRUE)
  )
  r <- types[info$type]
  stopifnot(!is.null(r))
  r
}

#'
#' fetches the data and returns data frame
#'
#' @param epidatacall and instance of EpiDataCall
#' @param fields filter fields
#' @importFrom readr read_csv
#' @importFrom httr RETRY stop_for_status content
#' @importFrom MMWRweek MMWRweek2Date
#' @return tibble
#'
#' @export
fetch_tbl <- function(epidatacall, fields = NULL, disable_date_parsing = FALSE) {
  r <- fetch_csv(epidatacall, fields)
  meta <- epidatacall$meta
  fields_pred = fields_to_predicate(fields)
  col_names = c()
  col_types = list()
  for(i in 1:length(meta)) {
    info = meta[[i]]
    if(fields_pred(info$name)) {
      col_names = c(col_names, info$name)
      col_types[info$name] = info_to_type(info, disable_date_parsing)
    }
  }
  tbl <- if(length(col_names) > 0) {
    readr::read_csv(r, col_types = col_types)
  } else {
    readr::read_csv(r)
  }


  if(!disable_date_parsing) {
    # parse weeks
    columns = colnames(tbl)
    for(i in 1:length(meta)) {
      info = meta[[i]]
      if(info$name %in% columns && info$type == 'epiweek') {
        tbl[[info$name]] = parse_api_week(tbl[[info$name]])
      }
    }
  }
  tbl
}


#'
#' fetches the data and returns data frame
#'
#' @param epidatacall and instance of EpiDataCall
#' @param fields filter fields
#' @importFrom readr read_csv
#' @importFrom httr RETRY stop_for_status content
#' @importFrom MMWRweek MMWRweek2Date
#' @return data.frame
#'
#' @export
fetch_df <- function(epidatacall, fields = NULL, disable_date_parsing = FALSE) {
  as.data.frame(fetch_tbl(epidatacall, fields, disable_date_parsing))
}
