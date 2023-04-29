join_url <- function(url, endpoint) {
  if (url[length(url)] != "/") {
    url <- paste0(url, "/")
  }
  paste0(url, endpoint)
}

#' performs the request
#' 
#' You can test the authentication headers like so:
#' \dontrun{
#' response <- httr::RETRY("GET", "https://httpbin.org/headers", httr::authenticate("epidata", get_auth_key()))
#' content(response)$headers$Authorization == paste0("Basic ", base64enc::base64encode(charToRaw("epidata:")))
#' }
#' 
#' @importFrom httr RETRY
do_request <- function(url, params) {
  # don't retry in case of certain status codes
  res <- httr::RETRY("GET",
    url = url,
    query = params,
    terminate_on = c(400, 401, 403, 405, 414, 500),
    http_headers,
    httr::authenticate("epidata", get_auth_key())
  )
  if (res$status_code == 414) {
    res <- httr::RETRY("POST",
      url = url,
      body = params,
      encode = "form",
      terminate_on = c(400, 401, 403, 405, 414, 500),
      http_headers,
      httr::authenticate("epidata", get_auth_key())
    )
  }
  res
}
