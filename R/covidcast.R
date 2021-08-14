

as_web_link <- function(obj) {

}

#'
#' creates the covidcast epidata helper
#'
#' @param base_url optional alternative base url
#' @importFrom httr RETRY stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom rlang abort
#' @return TODO
#'
#' @export
covidcast_epidata <- function(base_url = global_base_url) {
    url <- join_url(base_url, 'covidcast/meta')
    res <- do_request(url, list())

    httr::stop_for_status(res)
    r <- httr::content(res, "text", encoding = "UTF-8")
    meta <- jsonlite::fromJSON(r, simplifyVector=FALSE)

    meta
}