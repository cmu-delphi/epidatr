# Functions for performing the HTTP part of the API request.

#' Performs an API request and returns the response body as a string.
#'
#' Handles authentication, retries, 414 URI Too Long fallback to POST,
#' HTTP errors, and API-level errors. The API returns errors as JSON
#' regardless of the requested format, so for non-classic formats we
#' sniff the body for a JSON error response.
#'
#' @param epidata_call an instance of `epidata_call`
#' @param format_type format to request, one of "json", "csv", "classic"
#' @param timeout_seconds the maximum time to wait for a response
#' @param fields fields to include in the response, or NULL for all
#' @param http_method HTTP method to use
#' @param stream_threshold_bytes successful response bodies larger than this
#'   (per the `Content-Length` header, or when that header is absent) are
#'   streamed to a temp file instead of being read into memory.
#'   `Inf` always reads into memory
#' @return an in-memory `httr2_response`. When the body was streamed to disk it
#'   carries an extra `body_path` element (the temp file path) and an empty
#'   `body`; otherwise `body_path` is `NULL` and the body is in `body`.
#'
#' @importFrom httr2 req_perform req_perform_connection req_timeout req_headers
#' @importFrom httr2 req_user_agent req_retry resp_stream_raw resp_header
#' @importFrom httr2 req_error req_auth_basic resp_status req_method
#' @importFrom httr2 req_body_form req_url req_url_query
#' @importFrom rlang %||%
#' @importFrom xml2 read_html xml_find_all xml_text
#' @keywords internal
do_request <- function(epidata_call, format_type = c("json", "csv", "classic"), timeout_seconds, fields,
                       http_method = c("GET", "POST"), stream_threshold_bytes = Inf) {
  stopifnot(inherits(epidata_call, "epidata_call"))
  format_type <- rlang::arg_match(format_type)
  http_method <- rlang::arg_match(http_method)

  # Add any extra arguments to the request, such as fields to include in the response or format type.
  epidata_call <- extra_arguments(epidata_call, format_type, fields)
  req <- epidata_call$request

  # Add API key if it exists in environment variable
  key <- get_api_key()
  if (key != "") {
    if (epidata_call$api_version == "cast") {
      req <- req %>% httr2::req_headers(token = key)
    } else {
      req <- req %>% httr2::req_auth_basic("epidata", key)
    }
  }

  # Prepare the request with user agent, headers, timeout, and retry logic.
  req <- req %>%
    httr2::req_user_agent(paste0(
      "epidatr/",
      utils::packageVersion("epidatr")
    )) %>%
    httr2::req_headers(!!!http_headers) %>%
    httr2::req_timeout(timeout_seconds) %>%
    httr2::req_retry(
      max_tries = 3,
      is_transient = function(resp) {
        httr2::resp_status(resp) %in% c(
          429, # Too Many Requests
          500, # Internal Server Error
          502, # Bad Gateway
          503, # Service Unavailable
          504  # Gateway Timeout
        )

      }
    ) %>%
    # Use requested method.
    httr2::req_method(http_method) %>%
    httr2::req_error(is_error = function(resp) FALSE)

  # Do the request, streaming large bodies to disk to keep peak memory low.
  res <- perform_and_read(req, stream_threshold_bytes)

  # Fall back to POST if the request is too long for GET (414 URI Too Long).
  if (httr2::resp_status(res) == 414) {
    # 414 URI Too Long - Switch to POST
    query <- httr2::url_parse(httr2::req_url(req))$query
    req_post <- req %>%
      httr2::req_url_query() %>%
      httr2::req_method("POST") %>%
      httr2::req_body_form(!!!query)

    res <- perform_and_read(req_post, stream_threshold_bytes)
  }

  # If there is an error, extract the message from the API into the error
  # message if possible.
  if (httr2::resp_is_error(res)) {
    # 500, 429, 401 are possible
    msg <- "fetch data from API"
    if (identical(httr2::resp_content_type(res), "text/html") && httr2::resp_has_body(res)) {
      # grab the error information out of the returned HTML document
      msg <- paste(msg, ":", xml2::xml_text(xml2::xml_find_all(
        xml2::read_html(httr2::resp_body_string(res)),
        "//p"
      )))
    }
    httr2::resp_check_status(res, info = msg)
  }

  res
}

#' Perform a request, deciding whether to hold the body in memory or on disk.
#'
#' [httr2::req_perform_connection()] returns the headers with the
#' body still an open connection, so we can read `Content-Length` before pulling
#' the body. Small bodies and all error bodies are read into memory. Larger
#' ones or bodies with no `Content-Length` are streamed to a temp file a chunk
#' at a time to keep peak memory low.
#'
#' @param req an `httr2` request object
#' @param stream_threshold_bytes see [do_request()]
#' @return an `httr2_response`; see [do_request()] for the `body_path` element
perform_and_read <- function(req, stream_threshold_bytes = Inf) {
  resp <- httr2::req_perform_connection(req)
  drained <- FALSE
  # Close the response if it's not drained
  on.exit(if (!drained) close(resp), add = TRUE)

  status <- httr2::resp_status(resp)
  content_length <- suppressWarnings(as.numeric(httr2::resp_header(resp, "Content-Length") %||% NA))

  if (status < 400 && (is.na(content_length) || content_length > stream_threshold_bytes)) {
    path <- tempfile()
    con <- file(path, "wb")
    repeat {
      chunk <- httr2::resp_stream_raw(resp, kb = 1024)
      if (length(chunk) == 0) {
        break
      }
      writeBin(chunk, con)
    }
    close(con)
    close(resp)
    drained <- TRUE
    return(new_inmem_response(resp, raw(0), body_path = path))
  }

  # Drain the already-open connection rather than calling req_perform() to
  # avoid a second request.
  body_raw <- drain_to_raw(resp)
  close(resp)
  drained <- TRUE
  new_inmem_response(resp, body_raw)
}

# Read a streaming connection response fully into a raw vector.
drain_to_raw <- function(resp) {
  chunks <- list()
  repeat {
    chunk <- httr2::resp_stream_raw(resp, kb = 1024)
    if (length(chunk) == 0) {
      break
    }
    chunks[[length(chunks) + 1L]] <- chunk
  }
  # vctrs::list_unchop() is ~7x faster than do.call(c, chunks)
  if (length(chunks) == 0) raw(0) else vctrs::list_unchop(chunks)
}

# Rebuild an in-memory `httr2_response` from a (drained) connection response so
# `resp_body_*()` and `resp_check_status()` work as usual. `body_path` is an
# extra element carrying the temp-file path when the body was streamed to disk
# (in which case `body_raw` is empty); it is `NULL` otherwise. httr2 accessors
# read named fields and ignore the extra element.
new_inmem_response <- function(resp, body_raw, body_path = NULL) {
  structure(
    list(
      method = resp$method %||% "GET",
      url = resp$url,
      status_code = httr2::resp_status(resp),
      headers = resp$headers,
      body = body_raw,
      request = resp$request,
      cache = new.env(parent = emptyenv()),
      body_path = body_path
    ),
    class = "httr2_response"
  )
}
