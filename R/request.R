join_url <- function(url, endpoint) {
  if (url[length(url)] != "/") {
    url <- paste0(url, "/")
  }
  paste0(url, endpoint)
}

#' performs the request
#' @importFrom httr RETRY
do_request <- function(url, params) {
  # don't retry in case of certain status codes
  res <- httr::RETRY("GET", url,
    query = params, http_headers,
    terminate_on = c(400, 401, 403, 405, 414, 500)
  )
  if (res$status_code == 414) {
    res <- httr::RETRY("POST", url,
      body = params, encode = "form", http_headers,
      terminate_on = c(400, 401, 403, 405, 414, 500)
    )
  }
  res
}
