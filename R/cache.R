# IMPORTANT DEV NOTE:
# any file that manipulates this cache variable needs to be loaded after this, otherwise the superassignment operators will throw this in global!
# this is done by either only writing them in this file, or @include cache.R in the Roxygen docs
cache_environ <- new.env(parent = emptyenv())
cache_environ$use_cache <- NULL
cache_environ$epidatr_cache <- NULL
#' create a new cache for this session
#'
#' @description
#' `set_cache` (re)defines the cache to use. This does not clear existing data at any previous location, but defines a new access for this R session.
#' Say your cache is normally stored in the default directory, but for the current session you want to save your results in `~/my/temporary/savedirectory`, then you would call `set_cache(dir = "~/my/temporary/savedirectory")`.
#' Or if you know the data from 2 days ago is wrong, you could call `set_cache(days = 1)` to clear older data. In both cases, these changes would only last for a single session.
#' In general, it is better to set your preferences via environmental variables in your `.Renviron` folder, with the corresponding variables listed in the arguments section below.
#' In addition to those, there is the `EPIDATR_USE_CACHE` environmental variable, which unless defined to be `TRUE` otherwise defaults to `FALSE`.
#'
#' On the backend, the cache uses cachem, with filenames generated using an md5 encoding of the call url. Each file corresponds to a unique epidata-API call.
#' @examples
#' \dontrun{
#' set_cache(
#'   dir = "some/subdir",
#'   days = 14,
#'   max_size = 512,
#'   logfile = "some/subdir/logs.txt",
#'   prune_rate = 20L
#' )
#' }
#'
#' @param dir the directory in which the cache is stored. By default, this is `here::here(".epidatr_cache")`. The environmental variable is `EPIDATR_CACHE_DIR`
#' @param days the maximum length of time in days to keep any particular cached call. By default this is `7`
#' @param max_size the size of the entire cache, in MB, at which to start pruning entries.
#' @param logfile where cachem's log of transactions is stored. By default, it is `file.path(dir, "logfile.txt")`, so it's contained in the cache's directory. The environmental variable is `EPIDATR_CACHE_LOGFILE`
#' @param prune_rate how many calls to go between checking if any cache elements are too old or if the cache overall is too large. Defaults to `2000L`. Since cachem fixes the max time between prune checks to 5 seconds, there's little reason to actually change this parameter. Doesn't have a corresponding environmental variable.
#' @export
#' @import cachem
<<<<<<< Updated upstream
set_cache <- function(cache_dir = NULL,
=======
>>>>>>> Stashed changes
                      days = NULL,
                      max_size = NULL,
                      logfile = NULL,
                      prune_rate = 2000L) {
  if (is.null(cache_dir)) {
    cache_dir <- Sys.getenv("EPIDATR_CACHE_DIR", unset = here::here(".epidatr_cache"))
  }
  stopifnot(is.character(cache_dir))
  if (is.null(days)) {
    days <- Sys.getenv("EPIDATR_CACHE_MAX_AGE_DAYS", unset = 7) %>% as.numeric()
  }
  if (is.null(max_size)) {
    max_size <- Sys.getenv("EPIDATR_CACHE_MAX_SIZE_MB", unset = 1024) %>% as.numeric()
  }
  if (is.null(logfile)) {
    logfile <- Sys.getenv("EPIDATR_CACHE_LOGFILE", unset = file.path(cache_dir, "logfile.txt"))
  }
  stopifnot(is.character(logfile))
  stopifnot(is.numeric(days), is.numeric(max_size), is.integer(prune_rate))
  #
  # make sure that that directory exists and drag the user into that process
  cache_exists <- file.exists(cache_dir)
  cache_usable <- file.access(cache_dir, mode = 6) == 0
  if (!(cache_exists)) {
    user_input <- readline(glue::glue("there is no directory at {cache_dir}; the cache will be turned off until a viable directory has been set. Create one? (yes|no) "))
    repeat {
      valid_user_input <- ifelse(grepl("yes|no", user_input), sub(".*(yes|no).*", "\\1", user_input), NA)
      if (!is.na(valid_user_input)) {
        break
      }
      user_input <- readline(glue::glue(" please answer either yes or no: "))
    }
    if (valid_user_input == "yes") {
      dir.create(cache_dir, showWarnings = TRUE, recursive = TRUE)
      cache_exists <- TRUE
      cache_usable <- file.access(cache_dir, mode = 6) == 0
    }
  }


  if (!cache_usable) {
    print(glue::glue("The directory at {cache_dir} is not accessible; check permissions and/or use a different directory for the cache (see the `set_cache` documentation)."))
  } else if (cache_exists) {
    cache_environ$epidatr_cache <- cachem::cache_disk(
      dir = cache_dir,
      max_size = as.integer(max_size * 1024^2),
      max_age = days * 24 * 60 * 60,
      logfile = logfile,
      prune_rate = prune_rate
    )
  }
}


#' manually reset the cache, deleting all currently saved data and starting afresh
#'
#' @description
#' deletes the current cache and resets a new cache. Deletes local data! If you are using a session unique cache, you will have to pass the arguments you used for `set_cache` earlier, otherwise the system-wide `.Renviron`-based defaults will be used.
#' @examples
#' \dontrun{
#' clear_cache(
#'   dir = "some/subdir",
#'   days = 14,
#'   max_size = 512,
#'   logfile = "some/subdir/logs.txt",
#'   prune_rate = 20L
#' )
#' }
#'
#' @inheritParams set_cache
#' @export
clear_cache <- function(...) {
  cache_environ$epidatr_cache$destroy()
  set_cache(...)
}

#' turn off the caching for this session
#' @description
#' Disable caching until you call `set_cache` or restart R. The files defining the cache are untouched. If you are looking to disable the caching more permanently, set `EPIDATR_USE_CACHE=FALSE` as environmental variable in your `.Renviron`.
#' @export
disable_cache <- function() {
  cache_environ$epidatr_cache <- NULL
}

#' turn off the caching for this session
#' @description
#' Print out the information about the cache (as would be returned by cachem's `info()` method)
#' @export
cache_info <- function() {
  cache_environ$epidatr_cache$info()
}

<<<<<<< Updated upstream
=======
cache_epidata_call <- function(call, ...) {
#' turn off the caching for this session
#' @description
#' Print out the information about the cache (as would be returned by cachem's `info()` method)
#' @export
cache_info <- function() {
  cache_environ$epidatr_cache$info()
}

>>>>>>> Stashed changes
#' create a new cache for this session
#'
#' @description
#' the guts of caching, its interposed between fetch and the specific fetch methods. Internal method only.
#'
#' @param call the `epidata_call` object
#' @inheritParams fetch
#' @import cachem openssl
cache_epidata_call <- function(epidata_call, ...) {
  if (cache_environ$use_cache && !is.null(cache_environ$epidatr_cache)) {
    target <- request_url(epidata_call)
    hashed <- md5(target)
    cached <- cache_environ$epidatr_cache$get(hashed)
    if (!is.key_missing(cached)) {
      return(cached)
    }
  }
  # need to actually get the data, since its either not in the cache or we're not caching
  if (epidata_call$only_supports_classic) {
    fetched <- fetch_classic(epidata_call, ...)
  } else {
    fetched <- fetch_tbl(epidata_call, ...)
  }
  # add it to the cache if appropriate
  if (cache_environ$use_cache && !is.null(cache_environ$epidatr_cache)) {
    cache_environ$epidatr_cache$set(hashed, fetched)
  }
  return(fetched)
}
