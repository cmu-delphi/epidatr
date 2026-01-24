#' performs the request
#'
#' @importFrom httr2 req_perform req_timeout req_headers req_user_agent req_retry
#' @importFrom httr2 req_error req_auth_basic resp_status req_method
#' @importFrom httr2 req_body_form req_url req_url_query
#' @keywords internal
do_request <- function(epidata_call, timeout_seconds) {
  req <- epidata_call$request %>%
    httr2::req_user_agent(paste0("epidatr/", utils::packageVersion("epidatr"))) %>%
    httr2::req_headers(!!!http_headers) %>%
    httr2::req_timeout(timeout_seconds) %>%
    httr2::req_retry(
      max_tries = 3,
      is_transient = function(resp) {
        !httr2::resp_status(resp) %in% c(400, 401, 403, 405, 414, 500)
      }
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE)

  key <- get_api_key()
  if (key != "") {
    req <- req %>% httr2::req_auth_basic("epidata", key)
  }

  res <- httr2::req_perform(req)

  if (httr2::resp_status(res) == 414) {
    # 414 URI Too Long - Switch to POST
    query <- httr2::url_parse(httr2::req_url(req))$query
    req_post <- req %>%
      httr2::req_url_query() %>%
      httr2::req_method("POST") %>%
      httr2::req_body_form(!!!query)

    res <- httr2::req_perform(req_post)
  }

  res
}
