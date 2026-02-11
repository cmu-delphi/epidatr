to_httr2_response <- function(obj) {
  if (inherits(obj, "httr2_response")) {
    return(obj)
  }

  # Default structure for httr2_response
  new_obj <- list(
    method = "GET",
    url = "https://example.com/mocked",
    status_code = 200L,
    headers = list("content-type" = "application/json"),
    body = raw(0)
  )

  if (inherits(obj, "response")) { # httr response
    new_obj$url <- obj$url
    new_obj$status_code <- as.integer(obj$status_code)
    new_obj$headers <- as.list(obj$headers)
    new_obj$body <- obj$content
  } else if (is.character(obj) && length(obj) == 1) { # Raw JSON string
    new_obj$body <- charToRaw(obj)
  } else {
    stop("Unknown mock object type: ", class(obj)[1])
  }

  structure(new_obj, class = "httr2_response")
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
      body = body
    ),
    class = "httr2_response"
  )
}
