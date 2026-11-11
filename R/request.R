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
#' @return an `httr2_response` object
#'
#' @importFrom httr2 req_perform req_timeout req_headers req_user_agent req_retry
#' @importFrom httr2 req_error req_auth_basic resp_status req_method
#' @importFrom httr2 req_body_form req_url req_url_query
#' @importFrom xml2 read_html xml_find_all xml_text
#' @keywords internal
do_request <- function(
  epidata_call,
  format_type = c("json", "csv", "classic"),
  timeout_seconds,
  fields,
  http_method = c("GET", "POST")
) {
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
  max_wait <- max_retry_wait()
  req <- req %>%
    httr2::req_user_agent(paste0(
      "epidatr/",
      utils::packageVersion("epidatr")
    )) %>%
    httr2::req_headers(!!!http_headers) %>%
    httr2::req_timeout(timeout_seconds) %>%
    httr2::req_retry(
      max_tries = 3,
      is_transient = function(resp) is_transient_response(resp, max_wait),
      after = retry_after_seconds
    ) %>%
    # Use requested method.
    httr2::req_method(http_method) %>%
    httr2::req_error(is_error = function(resp) FALSE, body = error_body_message)

  # Do the request.
  res <- httr2::req_perform(req)

  # Fall back to POST if the request is too long for GET (414 URI Too Long).
  if (httr2::resp_status(res) == 414) {
    # 414 URI Too Long - Switch to POST
    query <- httr2::url_parse(httr2::req_get_url(req))$query
    req_post <- req %>%
      httr2::req_url(sub("\\?.*$", "", httr2::req_get_url(req))) %>%
      httr2::req_method("POST") %>%
      httr2::req_body_form(!!!query)

    res <- httr2::req_perform(req_post)
  }

  # Error message extraction is handled by error_body_message().
  httr2::resp_check_status(res)

  res
}

#' Retry policy for transient HTTP errors
#'
#' `is_transient_response()` decides whether `httr2::req_retry()` should retry
#' a response. Server and gateway errors (500, 502, 503, 504) are retried, and
#' a 429 only when the server says when to come back (the API's rate limits
#' are fixed windows, so retrying a few seconds later would fail again). Any
#' response asking us to wait longer than `max_wait` seconds is not retried:
#' the API answers 503 with `Retry-After: 3600` while a table is being
#' rebuilt, and sleeping for an hour, twice, is worse than failing with the
#' server's message.
#'
#' `retry_after_seconds()` parses the `Retry-After` header. It is used both as
#' the policy's input and as `req_retry()`'s `after` callback, so the two agree.
#' It returns `NA` (meaning: use httr2's default backoff) for a missing,
#' malformed, negative, or non-finite value, and never errors: an error inside
#' the retry loop could leave a streaming connection open.
#'
#' @param resp an `httr2_response`
#' @param max_wait the longest server-requested wait to honour, in seconds
#' @keywords internal
#' @noRd
is_transient_response <- function(resp, max_wait) {
  after <- retry_after_seconds(resp)
  if (isTRUE(after > max_wait)) {
    return(FALSE)
  }
  status <- httr2::resp_status(resp)
  if (status == 429) {
    return(!is.na(after))
  }
  status %in%
    c(
      500, # Internal Server Error
      502, # Bad Gateway
      503, # Service Unavailable
      504 # Gateway Timeout
    )
}

#' @noRd
retry_after_seconds <- function(resp) {
  after <- tryCatch(
    suppressWarnings(httr2::resp_retry_after(resp)),
    error = function(e) NA_real_
  )
  if (!is.numeric(after) || length(after) != 1 || !is.finite(after) || after < 0) {
    return(NA_real_)
  }
  as.numeric(after)
}

#' The longest server-requested retry wait to honour, from the
#' `epidatr.max_retry_wait` option (default 60 seconds).
#' @keywords internal
#' @noRd
max_retry_wait <- function() {
  value <- getOption("epidatr.max_retry_wait", 60)
  if (!is.numeric(value) || length(value) != 1 || !is.finite(value) || value < 0) {
    cli::cli_abort(
      "The {.code epidatr.max_retry_wait} option must be a single finite,
      non-negative number of seconds.",
      class = "epidatr__invalid_option"
    )
  }
  as.numeric(value)
}

#' A sentence saying when the server suggests retrying, or NULL if it gave no
#' usable `Retry-After` (or an implausibly long one).
#' @keywords internal
#' @noRd
retry_hint <- function(resp) {
  after <- retry_after_seconds(resp)
  if (is.na(after) || after <= 0 || after > 24 * 60 * 60) {
    return(NULL)
  }
  plural <- function(n, unit) paste(n, if (n == 1) unit else paste0(unit, "s"))
  seconds <- max(1, round(after))
  minutes <- round(after / 60)
  wait <- if (seconds < 60) {
    plural(seconds, "second")
  } else if (minutes < 90) {
    plural(minutes, "minute")
  } else {
    plural(round(minutes / 60), "hour")
  }
  paste0("The server suggests retrying in about ", wait, ".")
}

# Builds the detail shown under httr2's HTTP error message. httr2 shows a
# single string as one bullet, so the server's message and any retry hint are
# combined into one string.
error_body_message <- function(resp) {
  message <- error_body_detail(resp)
  if (length(message) > 0) {
    message <- paste(message, collapse = " ")
  }
  hint <- tryCatch(retry_hint(resp), error = function(e) NULL)
  parts <- c(message, hint)
  if (length(parts) == 0) {
    return(NULL)
  }
  paste(parts, collapse = " ")
}

error_body_detail <- function(resp) {
  tryCatch(
    {
      if (!httr2::resp_has_body(resp)) {
        return(NULL)
      }
      content_type <- httr2::resp_content_type(resp)
      if (identical(content_type, "application/json")) {
        # Regardless of the requested format type, an error response's body is
        # JSON, in one of two shapes: {"message": "..."} or FastAPI's
        # automatic validation errors under "detail"
        body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
        if (is.character(body$message) && length(body$message) == 1) {
          return(body$message)
        }
        detail <- body$detail
        if (is.character(detail) && length(detail) == 1) {
          return(detail)
        }
        if (is.list(detail) && length(detail) > 0) {
          return(paste(
            vapply(
              detail,
              function(d) {
                if (is.character(d$msg) && length(d$msg) == 1) d$msg else "invalid value"
              },
              character(1)
            ),
            collapse = "; "
          ))
        }
        NULL
      } else if (identical(content_type, "text/html")) {
        # grab the error information out of the returned HTML document
        xml2::xml_text(xml2::xml_find_all(
          xml2::read_html(httr2::resp_body_string(resp)),
          "//p"
        ))
      } else {
        NULL
      }
    },
    error = function(e) NULL
  )
}
