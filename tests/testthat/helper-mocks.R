# lightweight httr2_response constructors for mocking API calls.
# Used by test-epidatacall.R, test-covidcast.R, and test-cache.R.

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
      body = body
    ),
    class = "httr2_response"
  )
}
