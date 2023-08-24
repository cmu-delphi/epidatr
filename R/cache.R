# IMPORTANT DEV NOTE:
# any file that manipulates this cache variable needs to be loaded after this, otherwise the superassignment operators will throw this in global!
# this is done by either only writing them in this file, or @include cache.R in the Roxygen docs
cache_environ <- new.env(parent = emptyenv())
cache_environ$use_cache <- NULL
cache_environ$epidatr_cache <- NULL
#' create a new cache for this session
#' @rdname set_cache
#' @aliases foob
#'
#' @description
#' to reuse this cache, the environmental variables need to be set
#' Due to the 80 character limit on filenames in cachem, the cache for a given api call will be stored in the `md5` hash of the filename. For example,
#'
#' @examples
#' set_cache()
#'
#' @param dir
#' @param days
#' @param max_size
#' @param logfile
#' @param prune_rate
set_cache <- function(dir = NULL,
                      days = NULL,
                      max_size = NULL,
                      logfile = NULL,
                      prune_rate = 2000L) {
  if (is.null(dir)) {
    dir <- Sys.getenv("EPIDATR_CACHE_DIR", unset = here::here(".epidatr_cache"))
  }
  stopifnot(is.character(dir))
  if (is.null(days)) {
    days <- Sys.getenv("EPIDATR_CACHE_MAX_AGE_DAYS", unset = 7) %>% as.integer()
  }
  if (is.null(max_size)) {
    max_size <- Sys.getenv("EPIDATR_CACHE_MAX_SIZE_BYTES", unset = 1024^3) %>% as.integer()
  }
  if (is.null(logfile)) {
    logfile <- Sys.getenv("EPIDATR_CACHE_LOGFILE", unset = file.path(dir, "logfile.txt"))
  }
  stopifnot(is.character(logfile))
  stopifnot(is.integer(days), is.integer(max_size), is.integer(prune_rate))
  dir <- Sys.getenv("EPIDATR_CACHE_DIR", unset = here::here(".epidatr_cache"))
  cache_days <- Sys.getenv("")
  cache_environ$epidatr_cache <<- cachem::cache_disk(
    max_size = max_size,
    max_age = days * 24 * 60 * 60,
    logfile = logfile,
    prune_rate = prune_rate
  )
}

#' to manually reset the cache, deleting the currently saved data and starting fresh, call `epidatr_cache$destroy()`
clear_cache <- function(...) {
  cache_environ$epidatr_cache$destroy()
  set_cache(...)
}

disable_cache <- function() {
  cache_environ$epidatr_cache <<- NULL
}

cache_epidata_call <- function(call, ...) {
  if (cache_environ$use_cache && !is.null(cache_environ$epidatr_cache)) {
    target <- request_url(call)
    hashed <- md5(target)
    cached <- cache_environ$epidatr_cache$get(hashed)
    if (is.key_missing(cached)) {
      if (epidata_call$only_supports_classic) {
        fetched <- fetch_classic(epidata_call, ...)
      } else {
        fetched <- fetch_tbl(epidata_call, ...)
      }
      cache_environ$epidatr_cache$set(hashed, fetched)
      return(fetched)
    } else {
      return(cached)
    }
  } else {
    if (epidata_call$only_supports_classic) {
      return(fetch_classic(epidata_call, fields, return_empty = return_empty, timeout_seconds = timeout_seconds))
    } else {
      return(fetch_tbl(epidata_call, fields, disable_date_parsing, return_empty, timeout_seconds = timeout_seconds))
    }
  }
}
