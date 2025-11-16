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
#' @examples
#' \dontrun{
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
#' }
#'
#' @param endpoint the epidata endpoint to call
#' @param params the parameters to pass to the epidata endpoint
#' @param meta meta data to attach to the epidata call
#' @param only_supports_classic if true only classic format is supported
#'
#' @return
#' - For `create_epidata_call`: an `epidata_call` object
#'
#' @importFrom purrr map_chr map_lgl
create_epidata_call <- function(endpoint, params, meta = NULL,
                                only_supports_classic = FALSE) {
  checkmate::assert_character(endpoint, len = 1)
  checkmate::assert_list(params)
  checkmate::assert_list(meta, null.ok = TRUE)
  checkmate::assert_logical(only_supports_classic, len = 1)
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
  structure(
    list(
      endpoint = endpoint,
      params = params,
      base_url = global_base_url,
      meta = meta,
      only_supports_classic = only_supports_classic
    ),
    class = "epidata_call"
  )
}

#' @importFrom checkmate test_class test_list
request_arguments <- function(epidata_call, format_type, fields) {
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
  all_params <- c(epidata_call$params, extra_params)

  formatted_params <- list()
  for (name in names(all_params)) {
    v <- all_params[[name]]
    if (!is.null(v)) {
      if (test_class(v, "EpiRange")) {
        formatted_params[[name]] <- format_item(v)
      } else if (test_list(v)) {
        formatted_params[[name]] <- format_list(v)
      } else {
        formatted_params[[name]] <- format_item(v)
      }
    }
  }
  formatted_params
}

#' @export
print.epidata_call <- function(x, ...) {
  cli::cli_h1("<epidata_call> object:")
  cli::cli_bullets(c(
    "*" = "Pipe this object into `fetch()` to actually fetch the data",
    "*" = paste0("Request URL: ", request_url(x))
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
#' @details
#' `fetch` usually returns the data in tibble format, but a few of the
#' endpoints only support the JSON classic format (`pub_delphi`,
#' `pvt_meta_norostat`, and `pub_meta`). In that case a
#' JSON-like nested list structure is returned instead.
#'
#' @rdname epidata_call
#' @param epidata_call an instance of `epidata_call`
#' @param fetch_args a `fetch_args` object
#' @return
#' - For `fetch`: a tibble or a JSON-like list
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

  # Just display the epidata_call object, don't fetch the data
  if (fetch_args$dry_run) {
    return(epidata_call)
  }

  # Just display the raw response from the API, don't parse
  if (fetch_args$debug) {
    return(fetch_debug(epidata_call, fetch_args))
  }

  # Check if the data is cachable
  is_cachable <- check_is_cachable(epidata_call, fetch_args)
  if (is_cachable) {
    check_for_cache_warnings(epidata_call, fetch_args)

    # Check if the data is in the cache
    target <- request_url(epidata_call)
    hashed <- md5(target)
    cached <- cache_environ$epidatr_cache$get(hashed)
    if (!is.key_missing(cached)) {
      return(cached[[1]]) # extract `fetched` from `fetch()`, no metadata
    }
  }

  # Need to actually get the data, since its either not in the cache or we're not caching
  runtime <- system.time(if (epidata_call$only_supports_classic) {
    fetch_args[["disable_data_frame_parsing"]] <- TRUE
    fetched <- fetch_classic(epidata_call, fetch_args)
  } else {
    response_content <- fetch_classic(epidata_call, fetch_args = fetch_args)
    if (fetch_args$return_empty && length(response_content) == 0) {
      fetched <- tibble()
    } else {
      fetched <- parse_data_frame(
        epidata_call,
        response_content,
        fetch_args$disable_date_parsing,
        fetch_args$reference_week_day
      ) %>% as_tibble()
    }
  })

  # Add it to the cache if appropriate
  if (is_cachable || (fetch_args$refresh_cache && is_cache_enabled())) {
    cache_environ$epidatr_cache$set(hashed, list(fetched, Sys.time(), runtime))
  }

  return(fetched)
}

#' Fetches the data, raises on epidata errors, and returns the results as a
#' JSON-like list
#'
#' @rdname fetch_classic
#'
#' @param epidata_call an instance of `epidata_call`
#' @param fetch_args a `fetch_args` object
#' @importFrom httr stop_for_status content http_error
#' @importFrom jsonlite fromJSON
#' @return
#' - For `fetch_classic`: a JSON-like list
#' @keywords internal
fetch_classic <- function(epidata_call, fetch_args = fetch_args_list()) {
  stopifnot(inherits(epidata_call, "epidata_call"))
  stopifnot(inherits(fetch_args, "fetch_args"))

  response_content <- request_impl(epidata_call, "classic", fetch_args$timeout_seconds, fetch_args$fields) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyDataFrame = !fetch_args$disable_data_frame_parsing)

  # success is 1, no results is -2, truncated is 2, -1 is generic error
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

  response <- request_impl(epidata_call, fetch_args$format_type, fetch_args$timeout_seconds, fetch_args$fields)
  content <- httr::content(response, "text", encoding = "UTF-8")
  content
}

full_url <- function(epidata_call) {
  stopifnot(inherits(epidata_call, "epidata_call"))
  join_url(epidata_call$base_url, epidata_call$endpoint)
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
#' @importFrom httr modify_url
#' @return
#' - For `request_url`: string containing the URL
#' @keywords internal
request_url <- function(epidata_call, format_type = "classic", fields = NULL) {
  stopifnot(inherits(epidata_call, "epidata_call"))
  url <- full_url(epidata_call)
  params <- request_arguments(epidata_call, format_type, fields)
  httr::modify_url(url, query = params)
}

#' `epidata_call` object using a different base URL
#'
#' @param epidata_call an instance of `epidata_call`
#' @param base_url base URL to use
#' @return an `epidata_call` object
#' @keywords internal
with_base_url <- function(epidata_call, base_url) {
  stopifnot(inherits(epidata_call, "epidata_call"))
  stopifnot(is.character(base_url), length(base_url) == 1)
  epidata_call$base_url <- base_url
  epidata_call
}

#' Makes a request to the API and returns the response, catching
#' HTTP errors and forwarding the HTTP body in R errors
#' @importFrom httr stop_for_status content http_type
#' @importFrom xml2 read_html xml_find_all xml_text
#' @keywords internal
request_impl <- function(epidata_call, format_type, timeout_seconds, fields) {
  stopifnot(inherits(epidata_call, "epidata_call"))
  stopifnot(format_type %in% c("json", "csv", "classic"))

  url <- full_url(epidata_call)
  params <- request_arguments(epidata_call, format_type, fields)
  response <- do_request(url, params, timeout_seconds)

  if (response$status_code != 200) {
    # 500, 429, 401 are possible
    msg <- "fetch data from API"
    if (httr::http_type(response) == "text/html") {
      # grab the error information out of the returned HTML document
      msg <- paste(msg, ":", xml2::xml_text(xml2::xml_find_all(
        xml2::read_html(content(response, "text")),
        "//p"
      )))
    }
    httr::stop_for_status(response, task = msg)
  }

  response
}
