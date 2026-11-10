#' @section Package options:
#'
#' The `delphi.epidata.key` option specifies the API key to be used when making
#' requests to the Epidata API.
#'
#' The `epidatr.max_retry_wait` option (default 60) is the longest wait, in
#' seconds, that epidatr will honour when the API asks it to retry later (via a
#' `Retry-After` header). A request asked to wait longer fails right away with
#' the API's message, which says when to try again; for example, the API asks
#' for an hour while a data table is being rebuilt. This bounds each requested
#' wait, not the total time spent retrying. Set it to 0 to never wait for a
#' server-requested retry.
#'
#' @keywords internal
#' @include cache.R
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  cache_environ$use_cache <- as.logical(Sys.getenv(
    "EPIDATR_USE_CACHE",
    unset = FALSE
  ))
  if (is.na(cache_environ$use_cache)) {
    cli::cli_warn(
      "Failed to read EPIDATR_USE_CACHE environment variable.
        Should be a logical. Defaulting to FALSE."
    )
    cache_environ$use_cache <- FALSE
  }
  if (cache_environ$use_cache) {
    set_cache(startup = TRUE)
  }
}
