join_url <- function(url, endpoint) {
  if (!endsWith(url, "/")) {
    url <- paste0(url, "/")
  }
  paste0(url, endpoint)
}

#' performs the request
#'
#' You can test the authentication headers like so:
#' @examples
#' \dontrun{
#' response <- httr::RETRY(
#'   "GET", "https://httpbin.org/headers",
#'   httr::authenticate("epidata", "fake_key")
#' )
#' content(response)$headers$Authorization == paste0(
#'   "Basic ",
#'   base64enc::base64encode(charToRaw("epidata:fake_key"))
#' )
#' }
#'
#' @importFrom httr RETRY
#' @keywords internal
do_request <- function(url, params, timeout_seconds) {
  # don't retry in case of certain status codes
  key <- get_api_key()
  if (key != "") {
    res <- httr::RETRY("GET",
      url = url,
      query = params,
      terminate_on = c(400, 401, 403, 405, 414, 500),
      http_headers,
      httr::authenticate("epidata", get_api_key()),
      httr::timeout(timeout_seconds)
    )
  } else {
    res <- httr::RETRY("GET",
      url = url,
      query = params,
      terminate_on = c(400, 401, 403, 405, 414, 500),
      http_headers,
      httr::timeout(timeout_seconds)
    )
  }
  if (res$status_code == 414) {
    if (key != "") {
      res <- httr::RETRY("POST",
        url = url,
        body = params,
        encode = "form",
        terminate_on = c(400, 401, 403, 405, 414, 500),
        http_headers,
        httr::authenticate("epidata", get_api_key())
      )
    } else {
      res <- httr::RETRY("POST",
        url = url,
        body = params,
        encode = "form",
        terminate_on = c(400, 401, 403, 405, 414, 500),
        http_headers
      )
    }
  }
  res
}
