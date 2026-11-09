#' @section Package options:
#'
#' The `delphi.epidata.key` option specifies the API key to be used when making
#' requests to the Epidata API.
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
    # The user opted in via EPIDATR_USE_CACHE, so create a missing cache
    # directory instead of prompting during library() (#401).
    set_cache(startup = TRUE, confirm = FALSE)
  }
}
