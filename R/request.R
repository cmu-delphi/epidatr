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
#' @param stream_threshold_bytes the response body is accumulated in memory
#'   until this many bytes have been received, then it is streamed to
#'   a temp file to keep peak memory low. `Inf` always reads into memory.
#' @param download_path if not `NULL`, stream the (successful) response body to
#'   this path regardless of size, instead of a temp file
#' @return an `httr2_response` object
#'
#' @importFrom httr2 req_perform req_perform_connection req_timeout req_headers
#' @importFrom httr2 req_user_agent req_retry resp_stream_raw
#' @importFrom httr2 req_error req_auth_basic resp_status req_method
#' @importFrom httr2 req_body_form req_url req_url_query
#' @importFrom rlang %||%
#' @importFrom xml2 read_html xml_find_all xml_text
#' @keywords internal
do_request <- function(epidata_call,
                       format_type = c("json", "csv", "classic"),
                       timeout_seconds,
                       fields,
                       http_method = c("GET", "POST"),
                       stream_threshold_bytes = Inf,
                       download_path = NULL) {
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
          504 # Gateway Timeout
        )
      }
    ) %>%
    # Use requested method.
    httr2::req_method(http_method) %>%
    httr2::req_error(is_error = function(resp) FALSE)

  # Do the request
  res <- perform_and_read(req, stream_threshold_bytes, download_path)

  # Fall back to POST if the request is too long for GET (414 URI Too Long).
  if (httr2::resp_status(res) == 414) {
    # 414 URI Too Long - Switch to POST
    query <- httr2::url_parse(httr2::req_url(req))$query
    req_post <- req %>%
      httr2::req_url_query() %>%
      httr2::req_method("POST") %>%
      httr2::req_body_form(!!!query)

    res <- perform_and_read(req_post, stream_threshold_bytes, download_path)
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

#' Perform a request, holding the body in memory or spilling it to disk.
#'
#' The body is pulled a chunk at a time and kept in memory until it exceeds
#' `stream_threshold_bytes`, then moved to a temp file and streamed there.
#' Error bodies always stay in memory.
#'
#' @param req an `httr2` request object
#' @param stream_threshold_bytes see [do_request()]
#' @param download_path see [do_request()]
#' @return an `httr2_response`
#' @keywords internal
perform_and_read <- function(req,
                             stream_threshold_bytes = Inf,
                             download_path = NULL) {
  resp <- httr2::req_perform_connection(req)
  on.exit(close(resp), add = TRUE)

  # Errors are small and must stay in memory
  if (httr2::resp_is_error(resp)) {
    stream_threshold_bytes <- Inf
    download_path <- NULL
  }

  # Holds chunks in memory until they exceed the threshold,
  # then spilling to a file.
  # `con` is non-NULL once we have switched to writing on disk.
  chunks <- list()
  bytes <- 0
  path <- download_path
  con <- if (is.null(path)) NULL else file(path, "wb")

  # 8 MB chunks: benchmarking showed smaller chunks (e.g. 1 MB) spend most of
  # the time in per-call R<->curl overhead; 8 MB roughly halves streaming time
  # with negligible extra memory, and larger chunks give no further speedup.
  while (length(chunk <- httr2::resp_stream_raw(resp, kb = 1024L)) > 0) {
    if (!is.null(con)) {
      writeBin(chunk, con)
      next
    }
    chunks[[length(chunks) + 1L]] <- chunk
    # length() of a raw vector = number of bytes
    bytes <- bytes + length(chunk)
    if (bytes > stream_threshold_bytes) {
      path <- tempfile()
      con <- file(path, "wb")
      writeBin(vctrs::list_unchop(chunks), con) # flush what we have so far
      chunks <- NULL
    }
  }

  if (is.null(con)) {
    return(new_inmem_response(resp, vctrs::list_unchop(chunks) %||% raw(0)))
  }
  close(con)
  new_inmem_response(resp, raw(0), body_path = path)
}

# Rebuild an in-memory `httr2_response` with body_path argument to support streaming
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
