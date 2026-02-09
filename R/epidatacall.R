#' An abstraction that holds information needed to make an epidata request
#' @rdname epidata_call
#' @aliases epidata_call
#'
#' @description
#' `epidata_call` objects are generated internally by endpoint functions like
#'   [`pub_covidcast`]; by default, they are piped directly into the `fetch`
#'   function to fetch and format the data. For most endpoints this will return
#'   a tibble, but a few non-COVIDCAST endpoints will return a JSON-like list
#'   instead.
#'
#' @details
#' `create_epidata_call` is the constructor for `epidata_call` objects, but you
#'   should not need to use it directly; instead, use an endpoint function,
#'   e.g., [`pub_covidcast`], to generate an `epidata_call` for the data of
#'   interest.
#'
#' There are some other functions available for debugging and advanced usage: -
#'   `request_url` (for debugging):  outputs the request URL from which data
#'   would be fetched (note additional parameters below)
#'
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#' library(magrittr)
#'
#' call <- pub_covidcast(
#'   source = "jhu-csse",
#'   signals = "confirmed_7dav_incidence_prop",
#'   time_type = "day",
#'   geo_type = "state",
#'   time_values = epirange(20200601, 20200801),
#'   geo_values = c("ca", "fl"),
#'   fetch_args = fetch_args_list(dry_run = TRUE)
#' )
#' call %>% fetch()
#'
#' @param endpoint the epidata endpoint to call
#' @param params the parameters to pass to the epidata endpoint
#' @param meta meta data to attach to the epidata call
#'
#' @return
#' - For `create_epidata_call`: an `epidata_call` object
#'
#' @importFrom purrr map_chr map_lgl
create_epidata_call <- function(endpoint, params, meta = NULL) {
  checkmate::assert_character(endpoint, len = 1)
  checkmate::assert_list(params)
  checkmate::assert_list(meta, null.ok = TRUE)
  checkmate::assert_true(all(map_lgl(meta, ~ inherits(.x, "EpidataFieldInfo"))))

  if (length(unique(meta)) != length(meta)) {
    cli::cli_abort(
      c(
        "List of expected epidata fields contains duplicate entries",
        "i" = "duplicates in meta can cause problems parsing fetched data",
        "Please fix in `endpoints.R`"
      ),
      class = "epidatr__duplicate_meta_entries"
    )
  }

  meta_field_names <- map_chr(meta, "name")
  if (length(meta_field_names) != length(unique(meta_field_names))) {
    cli::cli_abort(
      c(
        "List of expected epidata fields contains duplicate names",
        "i" = "duplicates in meta can cause problems parsing fetched data",
        "Please fix in `endpoints.R`"
      ),
      class = "epidatr__duplicate_meta_names"
    )
  }

  # TODO: Check the categories in the future? We set up the categories
  # but we don't actually validate them yet?
  # use checkmate::assert_subset or something like that

  if (is.null(meta)) {
    meta <- list()
  }
  # Format the parameters before passing them to httr2::req_url_query
  # This is necessary because httr2::req_url_query expects atomic vector
  formatted_params <- format_params_for_api(params)

  r <- httr2::request(global_base_url) %>%
    httr2::req_url_path_append(endpoint) %>%
    httr2::req_url_query(!!!formatted_params, .multi = "comma")

  structure(
    list(
      request = r,
      base_url = global_base_url,
      meta = meta
    ),
    class = "epidata_call"
  )
}

#' @importFrom checkmate test_class test_list
format_params_for_api <- function(params) {
  # Remove NULL components
  params <- params[!vapply(params, is.null, logical(1))]

  lapply(params, function(v) {
    if (test_class(v, "EpiRange")) {
      format_item(v)
    } else if (test_list(v)) {
      format_list(v)
    } else {
      format_item(v)
    }
  })
}

#' @importFrom checkmate test_class test_list
extra_arguments <- function(epidata_call, format_type, fields) {
  stopifnot(inherits(epidata_call, "epidata_call"))
  stopifnot(format_type %in% c("json", "csv", "classic"))
  stopifnot(is.null(fields) || is.character(fields))

  extra_params <- list()
  if (format_type != "classic") {
    extra_params[["format"]] <- format_type
  }
  if (!is.null(fields)) {
    extra_params[["fields"]] <- fields
  }

  epidata_call$request <- epidata_call$request %>%
    httr2::req_url_query(!!!extra_params, .multi = "comma")

  epidata_call
}

#' @export
print.epidata_call <- function(x, ...) {
  cli::cli_h1("<epidata_call> object:")
  cli::cli_bullets(c(
    "*" = "Pipe this object into `fetch()` to actually fetch the data",
    "*" = paste0("Request URL: ", x$request$url)
  ))
}

#' Set custom API request parameters
#'
#' Used to specify custom options when making API requests, such as to set
#' timeouts or change data formats. These options are used by `fetch()` when it
#' makes calls to the Epidata API.
#'
#' @param ... not used for values, forces later arguments to bind by name
#' @param fields a list of epidata fields to return, or `NULL` to return all
#'   fields (default). e.g. `c("time_value", "value")` to return only the
#'   `time_value` and `value` fields or `c("-direction")` to return everything
#'   except the direction field
#' @param disable_date_parsing disable automatic date parsing
#' @param disable_data_frame_parsing disable automatic conversion to data frame;
#'   this is only supported by endpoints that only support the 'classic' format
#'   (non-tabular)
#' @param return_empty boolean that allows returning an empty tibble if there is
#'   no data
#' @param timeout_seconds the maximum amount of time (in seconds) to wait for a
#'   response from the API server
#' @param base_url base URL to use; by default `NULL`, which means the global
#'   base URL `"https://api.delphi.cmu.edu/epidata/"`
#' @param dry_run if `TRUE`, skip the call to the API and instead return the
#'   `epidata_call` object (useful for debugging)
#' @param debug if `TRUE`, return the raw response from the API
#' @param format_type the format to request from the API, one of classic, json,
#'   csv; this is only used by `fetch_debug`, and by default is `"json"`
#' @param refresh_cache if `TRUE`, ignore the cache, fetch the data from the
#'   API, and update the cache, if it is enabled
#' @param reference_week_day the day of the week to use as the reference day
#'   when parsing epiweeks to dates (happens if `disable_date_parsing` is `FALSE`)
#'   Defaults to 1 Sunday (the first day of the week).
#' @return A `fetch_args` object containing all the specified options
#' @export
#' @aliases fetch_args
#' @importFrom checkmate assert_character assert_logical assert_numeric
fetch_args_list <- function(
  ...,
  fields = NULL,
  disable_date_parsing = FALSE,
  disable_data_frame_parsing = FALSE,
  return_empty = FALSE,
  timeout_seconds = 15 * 60,
  base_url = NULL,
  dry_run = FALSE,
  debug = FALSE,
  format_type = c("json", "classic", "csv"),
  refresh_cache = FALSE,
  reference_week_day = 1
) {
  rlang::check_dots_empty()

  assert_character(fields, null.ok = TRUE, any.missing = FALSE)
  assert_logical(disable_date_parsing, null.ok = FALSE, len = 1L, any.missing = FALSE)
  assert_logical(disable_data_frame_parsing, null.ok = FALSE, len = 1L, any.missing = FALSE)
  assert_logical(return_empty, null.ok = FALSE, len = 1L, any.missing = FALSE)
  assert_numeric(timeout_seconds, null.ok = FALSE, len = 1L, any.missing = FALSE)
  assert_character(base_url, null.ok = TRUE, len = 1L, any.missing = FALSE)
  assert_logical(dry_run, null.ok = FALSE, len = 1L, any.missing = TRUE)
  assert_logical(debug, null.ok = FALSE, len = 1L, any.missing = FALSE)
  format_type <- match.arg(format_type)
  assert_logical(refresh_cache, null.ok = FALSE, len = 1L, any.missing = FALSE)
  assert_numeric(reference_week_day, null.ok = FALSE, len = 1L, any.missing = FALSE)

  structure(
    list(
      fields = fields,
      disable_date_parsing = disable_date_parsing,
      disable_data_frame_parsing = disable_data_frame_parsing,
      return_empty = return_empty,
      timeout_seconds = timeout_seconds,
      base_url = base_url,
      dry_run = dry_run,
      debug = debug,
      format_type = format_type,
      refresh_cache = refresh_cache,
      reference_week_day = reference_week_day
    ),
    class = "fetch_args"
  )
}

#' @export
print.fetch_args <- function(x, ...) {
  cli::cli_h1("<fetch_args> object:")
  # Print all non-class fields.
  cli::cli_dl(x[attr(x, "names")])
}

#' Fetches the data
#'
#' @rdname epidata_call
#' @param epidata_call an instance of `epidata_call`
#' @param fetch_args a `fetch_args` object
#' @return
#' - For `fetch`: a tibble
#' @export
#' @include cache.R
#' @importFrom openssl md5
#' @importFrom cachem is.key_missing
#' @importFrom tibble tibble as_tibble
#'
fetch <- function(epidata_call, fetch_args = fetch_args_list()) {
  stopifnot(inherits(epidata_call, "epidata_call"))
  stopifnot(inherits(fetch_args, "fetch_args"))

  if (!is.null(fetch_args$base_url)) {
    epidata_call <- with_base_url(epidata_call, fetch_args$base_url)
  }

  if (fetch_args$dry_run) {
    return(epidata_call)
  }

  if (fetch_args$debug) {
    return(fetch_debug(epidata_call, fetch_args))
  }

  # Check if the data is cachable
  is_cachable <- check_is_cachable(epidata_call, fetch_args)
  if (is_cachable) {
    check_for_cache_warnings(epidata_call, fetch_args)

    # Check if the data is in the cache
    target <- request_url(epidata_call, fetch_args$format_type, fetch_args$fields)
    hashed <- md5(target)
    cached <- cache_environ$epidatr_cache$get(hashed)
    if (!is.key_missing(cached)) {
      return(cached[[1]])
    }
  }

  runtime <- system.time({
    response_content <- request_epidata(epidata_call, fetch_args)

    if (fetch_args$return_empty && length(response_content) == 0) {
      fetched <- tibble()
    } else {
      fetched <- parse_data_frame(
        epidata_call,
        response_content,
        fetch_args$disable_date_parsing,
        fetch_args$reference_week_day
      ) |> as_tibble()
    }
  })

  # Add it to the cache if appropriate
  if (is_cachable || (fetch_args$refresh_cache && is_cache_enabled())) {
    cache_environ$epidatr_cache$set(hashed, list(fetched, Sys.time(), runtime))
  }

  return(fetched)
}

#' Fetches the data.
#'
#' Raises on errors from the API. Returns JSON.
#'
#' @rdname request_epidata
#'
#' @param epidata_call an instance of `epidata_call`
#' @param fetch_args a `fetch_args` object
#' @importFrom jsonlite fromJSON
#' @return
#' - For `request_epidata`: a JSON-like list
#' @keywords internal
request_epidata <- function(epidata_call, fetch_args = fetch_args_list(), simplify = TRUE) {
  stopifnot(inherits(epidata_call, "epidata_call"))
  stopifnot(inherits(fetch_args, "fetch_args"))

  if (!is.null(fetch_args$base_url)) {
    epidata_call <- with_base_url(epidata_call, fetch_args$base_url)
  }

  if (fetch_args$dry_run) {
    return(epidata_call)
  }

  response_content <- do_request(epidata_call, "classic", fetch_args$timeout_seconds, fetch_args$fields) %>%
    httr2::resp_body_string(encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyDataFrame = simplify)

  # Handle the case when there is an API error. Grab that messge. Success is 1,
  # no results is -2, truncated is 2, -1 is generic error.
  if (response_content$result != 1) {
    if ((response_content$result != -2) && !(fetch_args$return_empty)) {
      cli::cli_abort(
        c(
          "epidata error: {.code {response_content$message}}"
        ),
        class = "epidata_error"
      )
    }
  }

  if (response_content$message != "success") {
    cli::cli_warn(
      c(
        "epidata warning: {.code {response_content$message}}"
      ),
      class = "epidata_warning"
    )
  }

  return(response_content$epidata)
}

fetch_debug <- function(epidata_call, fetch_args = fetch_args_list()) {
  stopifnot(inherits(epidata_call, "epidata_call"))
  stopifnot(inherits(fetch_args, "fetch_args"))

  response <- do_request(epidata_call, fetch_args$format_type, fetch_args$timeout_seconds, fetch_args$fields)
  content <- httr2::resp_body_string(response, encoding = "UTF-8")
  content
}


#' Returns the full request url for the given epidata_call
#' @rdname request_url
#'
#' @param epidata_call an instance of `epidata_call`
#' @param format_type format to return one of classic,json,csv
#' @param fields a list of epidata fields to return, or NULL to return all
#'   fields (default) e.g. c("time_value", "value") to return only the
#'   time_value and value fields or c("-direction") to return everything except
#'   the direction field
#' @return
#' - For `request_url`: string containing the URL
#' @keywords internal
request_url <- function(epidata_call, format_type = "classic", fields = NULL) {
  stopifnot(inherits(epidata_call, "epidata_call"))

  epidata_call <- extra_arguments(epidata_call, format_type, fields)

  epidata_call$request$url
}

#' `epidata_call` object using a different base URL
#'
#' @param epidata_call an instance of `epidata_call`
#' @param base_url base URL to use
#' @return an `epidata_call` object
#' @importFrom httr2 url_modify url_parse
#' @importFrom stringr str_split
#' @keywords internal
with_base_url <- function(epidata_call, base_url) {
  stopifnot(inherits(epidata_call, "epidata_call"))
  stopifnot(is.character(base_url), length(base_url) == 1)

  # Extract http or https from base_url
  out <- stringr::str_split(base_url, "://")
  if (length(out[[1]]) == 2) {
    new_scheme <- out[[1]][1]
    new_hostname <- out[[1]][2]
  } else {
    new_scheme <- "https"
    new_hostname <- out[[1]][1]
  }

  epidata_call$request$url %>%
    httr2::url_modify(scheme = new_scheme, hostname=new_hostname) %>%
    httr2::url_parse()
}
