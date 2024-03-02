#' @section Package options:
#'
#' The `delphi.epidata.key` option specifies the API key to be used when making
#' requests to the Epidata API.
#'
#' @keywords internal
#' @include cache.R
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  cache_environ$use_cache <- Sys.getenv("EPIDATR_USE_CACHE", unset = FALSE)
  cache_environ$use_cache <- (cache_environ$use_cache == "TRUE")
  if (cache_environ$use_cache) {
    set_cache(startup = TRUE)
  }
}
