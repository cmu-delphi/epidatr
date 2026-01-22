join_url <- function(url, endpoint) {
  if (!endsWith(url, "/")) {
    url <- paste0(url, "/")
  }
  paste0(url, endpoint)
}

#' performs the request
#'
#' You can test the authentication headers like so:
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' response <- httr2::request("https://httpbin.org/headers") %>%
#'   httr2::req_auth_basic("epidata", "fake_key") %>%
#'   httr2::req_perform() %>%
#'   httr2::resp_body_json()
#' response$headers$Authorization == paste0(
#'   "Basic ",
#'   base64enc::base64encode(charToRaw("epidata:fake_key"))
#' )
#'
#' @importFrom httr2 request req_user_agent req_headers req_timeout req_retry req_error req_auth_basic req_url_query req_perform req_method req_body_form resp_status
#' @importFrom magrittr %>%
#' @keywords internal
do_request <- function(url, params, timeout_seconds) {
  # don't retry in case of certain status codes
  key <- get_api_key()

  req <- request(url) %>%
    req_user_agent(paste0("epidatr/", version)) %>%
    req_headers("Accept-Encoding" = "gzip") %>%
    req_timeout(timeout_seconds) %>%
    req_retry(
      max_tries = 3,
      is_transient = function(resp) {
        !(resp_status(resp) %in% c(400, 401, 403, 405, 414, 500))
      }
    ) %>%
    req_error(is_error = function(resp) FALSE)

  if (key != "") {
    req <- req %>% req_auth_basic("epidata", key)
  }

  # Try GET
  req_get <- req %>% req_url_query(!!!params)
  res <- req_perform(req_get)

  if (resp_status(res) == 414) {
    if (key != "") {
      # Auth is already in req
    }
    # Try POST
    req_post <- req %>%
      req_method("POST") %>%
      req_body_form(!!!params)

    res <- req_perform(req_post)
  }
  res
}
