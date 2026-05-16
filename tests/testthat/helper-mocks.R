# Convert a raw JSON/CSV string (or pass-through an existing httr2_response)
# into an httr2_response object suitable for mocking req_perform.
to_httr2_response <- function(obj) {
  if (inherits(obj, "httr2_response")) {
    return(obj)
  }
  if (!(is.character(obj) && length(obj) == 1)) {
    stop("Unknown mock object type: ", class(obj)[1])
  }
  structure(
    list(
      method = "GET",
      url = "https://example.com/mocked",
      status_code = 200L,
      headers = list("content-type" = "application/json"),
      body = charToRaw(obj),
      cache = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}

# Mock httr2::req_perform to return a fixed response. `response` can be an
# httr2_response (built via create_mock_response) or a raw JSON/CSV string.
with_mocked_response <- function(response, code) {
  resp <- to_httr2_response(response)
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) resp,
    .package = "httr2",
    .env = parent.frame()
  )
  force(code)
}

# Mock httr2::req_perform with a handler that receives the outgoing request,
# letting tests assert on URL/headers/body and vary the response per call.
with_mock_perform <- function(handler, code) {
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) to_httr2_response(handler(req)),
    .package = "httr2",
    .env = parent.frame()
  )
  force(code)
}

create_mock_response <- function(body,
                                 status_code = 200L,
                                 headers = list("content-type" = "application/json"),
                                 url = "https://example.com/mocked") {
  if (is.character(body) && length(body) == 1) {
    body <- charToRaw(body)
  }

  structure(
    list(
      method = "GET",
      url = url,
      status_code = as.integer(status_code),
      headers = headers,
      body = body,
      cache = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}
