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
#' @return the response body as a string
#'
#' @importFrom httr2 req_perform req_timeout req_headers req_user_agent req_retry
#' @importFrom httr2 req_error req_auth_basic resp_status req_method
#' @importFrom httr2 req_body_form req_url req_url_query
#' @importFrom xml2 read_html xml_find_all xml_text
#' @keywords internal
do_request <- function(epidata_call, format_type, timeout_seconds, fields,
                       http_method = c("GET", "POST")) {
  stopifnot(inherits(epidata_call, "epidata_call"))
  stopifnot(format_type %in% c("json", "csv", "classic"))
  http_method <- rlang::arg_match(http_method)

  # Add any extra arguments to the request, such as fields to include in the response or format type.
  epidata_call <- extra_arguments(epidata_call, format_type, fields)
  req <- epidata_call$request

  # Add API key if it exists in environment variable
  key <- get_api_key()
  if (key != "") {
    req <- req %>% httr2::req_auth_basic("epidata", key)
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
        !httr2::resp_status(resp) %in% c(400, 401, 403, 405, 414, 500)
      }
    ) %>%
    # Use requested method.
    httr2::req_method(http_method) %>%
    httr2::req_error(is_error = function(resp) FALSE)

  # Do the request.
  res <- httr2::req_perform(req)

  # Fall back to POST if the request is too long for GET (414 URI Too Long).
  if (httr2::resp_status(res) == 414) {
    # 414 URI Too Long - Switch to POST
    query <- httr2::url_parse(httr2::req_url(req))$query
    req_post <- req %>%
      httr2::req_url_query() %>%
      httr2::req_method("POST") %>%
      httr2::req_body_form(!!!query)

    res <- httr2::req_perform(req_post)
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

  body <- httr2::resp_body_string(res, encoding = "UTF-8")

  # The API returns errors as JSON regardless of requested format. If we asked
  # for non-classic (e.g. CSV) but got JSON back, it's an error response.
  if (format_type != "classic" && startsWith(body, "{")) {
    parsed <- jsonlite::fromJSON(body)
    check_epidata_result(parsed)
  }

  body
}

#' Check an API response for epidata-level errors and warnings.
#'
#' @param response_content parsed JSON response with `result` and `message` fields
#' @param allow_empty if TRUE, suppress errors for "no results" (result == -2)
#' @keywords internal
check_epidata_result <- function(response_content, allow_empty = FALSE) {
  # success is 1, no results is -2, truncated is 2, -1 is generic error
  if (response_content$result != 1) {
    if ((response_content$result != -2) && !allow_empty) {
      cli::cli_abort(
        "epidata error: {.code {response_content$message}}",
        class = "epidata_error"
      )
    }
  }

  if (response_content$message != "success") {
    cli::cli_warn(
      "epidata warning: {.code {response_content$message}}",
      class = "epidata_warning"
    )
  }
}
