# Functions for creating and handling `epidata_call` objects, which represent
# API calls to the Epidata API. The `fetch()` function is used to execute an
# `epidata_call` and return the data.

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
#' @param api_version string. The API version to use. One of "classic" or "cast".
#' @param response_format string. The expected format of the response. One of "classic", "json", or "csv".
#'
#' @return
#' - For `create_epidata_call`: an `epidata_call` object
#'
#' @importFrom purrr map_chr map_lgl
create_epidata_call <- function(endpoint, params, meta = NULL,
                                api_version = c("classic", "cast"),
                                response_format = c("classic", "json", "csv")) {
  checkmate::assert_character(endpoint, len = 1)
  checkmate::assert_list(params)
  checkmate::assert_list(meta, null.ok = TRUE)
  api_version <- rlang::arg_match(api_version)
  response_format <- rlang::arg_match(response_format)
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
  base_url <- if (api_version == "cast") cast_base_url else global_base_url
  # Format the parameters before passing them to httr2::req_url_query
  # This is necessary because httr2::req_url_query expects atomic vector
  formatted_params <- format_params_for_api(params)

  r <- httr2::request(base_url) %>%
    httr2::req_url_path_append(endpoint) %>%
    httr2::req_url_query(!!!formatted_params, .multi = "comma")

  structure(
    list(
      request = r,
      base_url = base_url,
      meta = meta,
      api_version = api_version,
      response_format = response_format
    ),
    class = "epidata_call"
  )
}

#' @importFrom checkmate test_class test_list
extra_arguments <- function(epidata_call, format_type, fields) {
  stopifnot(inherits(epidata_call, "epidata_call"))
  stopifnot(format_type %in% c("json", "csv", "classic"))
  stopifnot(is.null(fields) || is.character(fields))

  extra_params <- list()
  # The cast API rejects unknown query params (`extra_forbidden`); response
  # format is determined by the endpoint, and `fields` isn't supported.
  if (epidata_call$api_version != "cast") {
    if (format_type != "classic") {
      extra_params[["format"]] <- format_type
    }
    if (!is.null(fields)) {
      extra_params[["fields"]] <- fields
    }
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
#' @param stream_threshold_bytes for large responses, avoid high peak memory
#'   use during fetching by streaming the response body to a temp file instead
#'   of reading it into memory. Responses whose `Content-Length` exceeds this
#'   many bytes (or that report no `Content-Length`) are streamed to disk and
#'   parsed from there; smaller responses are read into memory as usual. The
#'   returned object is a tibble either way. Set to `Inf` to always read into
#'   memory. Defaults to 500 MiB.
#' @param download_path if not `NULL`, the raw response body is streamed to this
#'   file (regardless of size) and kept. Useful for very large pulls. If parsing
#'   into memory fails, the download is preserved here so you can read it with
#'   out-of-memory tools (e.g. `arrow` or `duckdb`) instead of re-downloading.
#' @param base_url base URL to use; by default `NULL`, which means the global
#'   base URL `"https://api.delphi.cmu.edu/epidata/"`
#' @param dry_run if `TRUE`, skip the call to the API and instead return the
#'   `epidata_call` object (useful for debugging)
#' @param refresh_cache if `TRUE`, ignore the cache, fetch the data from the
#'   API, and update the cache, if it is enabled
#' @param debug `r lifecycle::badge("deprecated")` No longer supported. Use `dry_run = TRUE` instead.
#' @param format_type `r lifecycle::badge("deprecated")` Now managed internally.
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
  stream_threshold_bytes = 500 * 1024^2,
  download_path = NULL,
  base_url = NULL,
  dry_run = FALSE,
  debug = lifecycle::deprecated(),
  format_type = lifecycle::deprecated(),
  refresh_cache = FALSE,
  reference_week_day = 1
) {
  # Deprecation warnings
  if (lifecycle::is_present(debug)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "fetch_args_list(debug)",
      details = "The `debug` argument is no longer supported. Use `dry_run = TRUE` instead."
    )
  }
  if (lifecycle::is_present(format_type)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "fetch_args_list(format_type)",
      details = "The `format_type` argument is now managed internally to ensure efficient data fetching."
    )
  }

  rlang::check_dots_empty()

  assert_character(fields, null.ok = TRUE, any.missing = FALSE)
  assert_logical(disable_date_parsing, null.ok = FALSE, len = 1L, any.missing = FALSE)
  assert_logical(disable_data_frame_parsing, null.ok = FALSE, len = 1L, any.missing = FALSE)
  assert_logical(return_empty, null.ok = FALSE, len = 1L, any.missing = FALSE)
  assert_numeric(timeout_seconds, null.ok = FALSE, len = 1L, any.missing = FALSE)
  assert_numeric(stream_threshold_bytes, null.ok = FALSE, len = 1L, any.missing = FALSE, lower = 0)
  assert_character(download_path, null.ok = TRUE, len = 1L, any.missing = FALSE)
  assert_character(base_url, null.ok = TRUE, len = 1L, any.missing = FALSE)
  assert_logical(dry_run, null.ok = FALSE, len = 1L, any.missing = TRUE)
  assert_logical(refresh_cache, null.ok = FALSE, len = 1L, any.missing = FALSE)
  assert_numeric(reference_week_day, null.ok = FALSE, len = 1L, any.missing = FALSE)

  structure(
    list(
      fields = fields,
      disable_date_parsing = disable_date_parsing,
      disable_data_frame_parsing = disable_data_frame_parsing,
      return_empty = return_empty,
      timeout_seconds = timeout_seconds,
      stream_threshold_bytes = stream_threshold_bytes,
      download_path = download_path,
      base_url = base_url,
      dry_run = dry_run,
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

  # If cacheable and the value is in cache, return the cached value.
  is_cachable <- check_is_cachable(epidata_call, fetch_args)
  should_write_cache <- is_cachable || (fetch_args$refresh_cache && is_cache_enabled())

  if (should_write_cache) {
    target <- request_url(epidata_call, "json", fetch_args$fields)
    hashed <- openssl::md5(target)
  }

  if (is_cachable) {
    check_for_cache_warnings(epidata_call, fetch_args)

    cached <- cache_environ$epidatr_cache$get(hashed)
    if (!is.key_missing(cached)) {
      return(cached[[1]])
    }
  }

  # Otherwise fetch the data from the API.
  runtime <- system.time({
    response_content <- request_epidata(epidata_call, fetch_args)

    if (fetch_args$return_empty && length(response_content) == 0) {
      fetched <- tibble::tibble()
    } else {
      fetched <- parse_data_frame(
        epidata_call,
        response_content,
        fetch_args$disable_date_parsing,
        fetch_args$reference_week_day
      ) %>% tibble::as_tibble()
    }
  })

  # Add to cache if appropriate.
  if (should_write_cache) {
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

  res <- do_request(
    epidata_call,
    format_type = epidata_call$response_format,
    timeout_seconds = fetch_args$timeout_seconds,
    fields = fetch_args$fields,
    stream_threshold_bytes = fetch_args$stream_threshold_bytes,
    download_path = fetch_args$download_path
  )
  # Whether the streamed file is the user's chosen download_path (keep it) or a
  # temp file (delete after a successful read).
  from_download_path <- !is.null(fetch_args$download_path)

  if (epidata_call$response_format == "csv") {
    if (!is.null(res$body_path)) {
      return(read_streamed_body(
        res$body_path,
        function(p) {
          readr::read_csv(p, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
        },
        from_download_path
      ))
    }
    return(readr::read_csv(I(httr2::resp_body_string(res)),
                           col_types = readr::cols(.default = "c"),
                           show_col_types = FALSE))
  }

  # JSON parsing (both "json" and "classic")
  response_content <- if (!is.null(res$body_path)) {
    read_streamed_body(
      res$body_path,
      function(p) jsonlite::fromJSON(p, simplifyVector = simplify, simplifyDataFrame = simplify),
      from_download_path
    )
  } else {
    httr2::resp_body_json(res, simplifyVector = simplify, simplifyDataFrame = simplify)
  }

  if (epidata_call$response_format == "json") {
    return(response_content)
  }

  # classic: JSON with result/message wrapper
  check_epidata_result(response_content, allow_empty = fetch_args$return_empty)
  return(response_content$epidata)
}

# Parse a response body that was streamed to disk.
read_streamed_body <- function(path, reader, from_download_path) {
  out <- tryCatch(
    reader(path),
    error = function(cnd) {
      size <- format(
        structure(file.size(path), class = "object_size"),
        units = "auto"
      )
      msg <- c(
        "Couldn't read the downloaded response into memory.",
        "i" = paste0("The raw response is at {.file {path}} (", size, ")."),
        "i" = paste(
          "Read it directly, e.g. with the {.pkg arrow} or {.pkg duckdb}",
          "packages for larger-than-memory data."
        )
      )
      if (!from_download_path) {
        msg <- c(msg, "i" = paste(
          "It is kept only for this R session. Pass {.arg download_path}",
          "to {.fn fetch_args_list} to save downloads persistently."
        ))
      }
      cli::cli_abort(msg, parent = cnd, class = "epidatr__read_download_failed")
    }
  )
  if (!from_download_path) {
    unlink(path)
  }
  out
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
#' @keywords internal
with_base_url <- function(epidata_call, base_url) {
  stopifnot(inherits(epidata_call, "epidata_call"))
  stopifnot(is.character(base_url), length(base_url) == 1)

  old_base_url <- epidata_call$base_url
  if (endsWith(old_base_url, "/") && !endsWith(base_url, "/")) {
    base_url <- paste0(base_url, "/")
  }

  epidata_call$request$url <- sub(
    old_base_url,
    base_url,
    epidata_call$request$url,
    fixed = TRUE
  )
  epidata_call$base_url <- base_url

  epidata_call
}
