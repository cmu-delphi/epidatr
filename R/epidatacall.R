
create_epidata_call <- function(endpoint, params) {
    stopifnot(is.character(endpoint), length(endpoint) == 1)
    stopifnot(is.list(params))
    structure(list(
        endpoint=endpoint,
        params=params
    ),
    class="EpiDataCall")
}

request_arguments <- function(epidatacall, format_type, fields = NULL) {
    stopifnot(inherits(epidatacall, 'EpiDataCall'))
    stopifnot(format_type %in% c('json', 'csv', 'classic'))
    stopifnot(is.null(fields) || is.character(fields))

    extra_params = list()
    if(format_type != 'classic') {
        extra_params[['format']] = format_type
    }
    if(!is.null(fields)) {
        extra_params[['fields']] = fields
    }
    all_params = c(epidatacall$params, extra_params)

    formatted_params = list()
    for(name in names(all_params)) {
        v = all_params[[name]]
        if (!is.null(v)) {
            formatted_params[[name]] = format_list(v)
        }
    }
    formatted_params
}

full_url <- function(epidatacall, base_url = BASE_URL) {
    stopifnot(inherits(epidatacall, 'EpiDataCall'))
    stopifnot(is.character(base_url), length(base_url) == 1)
    url = base_url
    if(url[length(url)] != '/') {
        url = paste0(url, '/')
    }
    paste0(url, epidatacall$endpoint)
}

request_impl <- function(epidatacall, format_type, base_url = BASE_URL, fields = NULL) {
    stopifnot(inherits(epidatacall, 'EpiDataCall'))
    stopifnot(format_type %in% c('json', 'csv', 'classic'))
    # API call
    url = full_url(epidatacall, base_url)
    params = request_arguments(epidatacall, format_type, fields)

    res <- httr::GET(url, query=params)
    if (res$status_code == 414) {
      res <- httr::POST(url, body=params, encode='form')
    }
    httr::content(res, 'text')
  }

#'
#' fetches the data and returns the classic format
#'
#' @param epidatacall and instance of EpiDataCall
#' @param base_url optional base url
#' @param fields filter fields
#' @return parsed json message
#'
#' @export
as_classic <- function(epidatacall, base_url = BASE_URL, fields = NULL) {
    r <- request_impl(epidatacall, 'classic', base_url, fields)
    jsonlite::fromJSON(r)
}

#'
#' fetches the data and returns the josn format
#'
#' @param epidatacall and instance of EpiDataCall
#' @param base_url optional base url
#' @param fields filter fields
#' @return parsed json message
#'
#' @export
as_json <- function(epidatacall, base_url = BASE_URL, fields = NULL) {
    r <- request_impl(epidatacall, 'json', base_url, fields)
    jsonlite::fromJSON(r)
}

#'
#' fetches the data and returns the CSV text
#'
#' @param epidatacall and instance of EpiDataCall
#' @param base_url optional base url
#' @param fields filter fields
#' @return CSV text
#'
#' @export
as_csv <- function(epidatacall, base_url = BASE_URL, fields = NULL) {
    request_impl(epidatacall, 'csv', base_url, fields)
}

#'
#' fetches the data and returns data frame
#'
#' @param epidatacall and instance of EpiDataCall
#' @param base_url optional base url
#' @param fields filter fields
#' @return data.frame
#'
#' @export
as_df <- function(epidatacall, base_url = BASE_URL, fields = NULL) {
    r <- as_csv(epidatacall, base_url, fields)
    readr::read_csv(r)
}
