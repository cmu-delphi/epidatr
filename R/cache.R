# IMPORTANT DEV NOTE: make sure to @include cache.R in the Roxygen docs of any
# function referencing this environment, so this file is loaded first
cache_environ <- new.env(parent = emptyenv())
cache_environ$use_cache <- NULL
cache_environ$epidatr_cache <- NULL
cache_environ$cache_args <- NULL

#' Create or renew a cache for this session
#' @aliases set_cache
#' @description
#' By default, epidatr re-requests data from the API on every call of `fetch`.
#'   In case you find yourself repeatedly calling the same data, you can enable
#'   the cache using either this function for a given session, or environmental
#'   variables for a persistent cache.
#' The typical recommended workflow for using the cache is to set the
#'   environmental variables `EPIDATR_USE_CACHE=TRUE` and
#'   `EPIDATR_CACHE_DIRECTORY="/your/directory/here"`in your `.Renviron`, for
#'   example by calling `usethis::edit_r_environ()`.
#' See the parameters below for some more configurables if you're so inclined.
#'
#' `set_cache` (re)defines the cache to use in a particular R session. This does
#'   not clear existing data at any previous location, but instead creates a
#'   handle to the new cache using [cachem](https://cachem.r-lib.org/index.html)
#'   that seamlessly handles caching for you.
#' Say your cache is normally stored in some default directory, but for the
#'   current session you want to save your results in
#'   `~/my/temporary/savedirectory`, then you would call `set_cache(dir =
#'   "~/my/temporary/savedirectory")`.
#' Or if you know the data from 2 days ago is wrong, you could call
#'   `set_cache(days = 1)` to clear older data whenever the cache is referenced.
#' In both cases, these changes would only last for a single session (though the
#'   deleted data would be gone permanently!).
#'
#' An important feature of the caching in this package is that only calls which
#'   specify either `issues` before a certain date, or `as_of` before a certain
#'   date will actually cache. For example the call
#' ```
#' pub_covidcast(
#'   source = "jhu-csse",
#'   signals = "confirmed_7dav_incidence_prop",
#'   geo_type = "state",
#'   time_type = "day",
#'   geo_values = "ca,fl",
#'   time_values = epirange(20200601, 20230801)
#' )
#' ```
#' *won't* cache, since it is possible for the cache to be invalidated by new
#'   releases with no warning. On the other hand, the call
#' ```
#' pub_covidcast(
#'   source = "jhu-csse",
#'   signals = "confirmed_7dav_incidence_prop",
#'   geo_type = "state",
#'   time_type = "day",
#'   geo_values = "ca,fl",
#'   time_values = epirange(20200601, 20230801),
#'   as_of = "2023-08-01"
#' )
#' ```
#' *will* cache, since normal new versions of data can't invalidate it (since
#'   they would be `as_of` a later date). It is still possible that Delphi may
#'   patch such data, but the frequency is on the order of months rather than
#'   days. We are working on creating a public channel to communicate such
#'   updates. While specifying `issues` will usually cache, a call with
#'   `issues="*"` won't cache, since its subject to cache invalidation by normal
#'   versioning.
#'
#' On the backend, the cache uses cachem, with filenames generated using an md5
#'   encoding of the call url. Each file corresponds to a unique epidata-API
#'   call.
#' @examples
#' set_cache(
#'   cache_dir = tempdir(),
#'   days = 14,
#'   max_size = 512,
#'   logfile = "logs.txt"
#' )
#'
#' @param cache_dir the directory in which the cache is stored. By default, this
#'   is `rappdirs::user_cache_dir("R", version = "epidatr")`. The path can be
#'   either relative or absolute. The environmental variable is
#'   `EPIDATR_CACHE_DIR`.
#' @param days the maximum length of time in days to keep any particular cached
#'   call. By default this is `1`. The environmental variable is
#'   `EPIDATR_CACHE_MAX_AGE_DAYS`.
#' @param max_size the size of the entire cache, in MB, at which to start
#'   pruning entries. By default this is `1024`, or 1GB. The environmental
#'   variable is `EPIDATR_CACHE_MAX_SIZE_MB`.
#' @param logfile where cachem's log of transactions is stored, relative to the
#'   cache directory. By default, it is `"logfile.txt"`. The environmental
#'   variable is `EPIDATR_CACHE_LOGFILE`.
#' @param confirm whether to confirm directory creation. default is `TRUE`;
#'   should only be set in non-interactive scripts
#' @param startup indicates whether the function is being called on
#'   startup. Affects suppressability of the messages. Default is `FALSE`.
#' @return [`NULL`] no return value, all effects are stored in the package
#'         environment
#' @seealso [`clear_cache`] to delete the old cache while making a new one,
#'   [`disable_cache`] to disable without deleting, and [`cache_info`]
#' @export
#' @import cachem
#' @import glue
#' @importFrom utils sessionInfo
set_cache <- function(cache_dir = NULL,
                      days = NULL,
                      max_size = NULL,
                      logfile = NULL,
                      confirm = TRUE,
                      startup = FALSE) {
  if (is.null(cache_dir)) {
    cache_dir <- Sys.getenv("EPIDATR_CACHE_DIR", unset = rappdirs::user_cache_dir("R", version = "epidatr"))
  } else if (is.null(cache_dir)) {
    # earlier version, so no tools
    cache_dir <- Sys.getenv("EPIDATR_CACHE_DIR")
    if (cache_dir == "") {
      cli::cli_abort("no valid EPIDATR_CACHE_DIR", class = "epidatr_cache_error")
    }
  }
  stopifnot(is.character(cache_dir))
  if (is.null(days)) {
    days <- Sys.getenv("EPIDATR_CACHE_MAX_AGE_DAYS", unset = 1) %>% as.numeric()
  }
  if (is.null(max_size)) {
    max_size <- Sys.getenv("EPIDATR_CACHE_MAX_SIZE_MB", unset = 1024) %>% as.numeric()
  }
  if (is.null(logfile)) {
    logfile <- Sys.getenv("EPIDATR_CACHE_LOGFILE", unset = "logfile.txt")
  }
  stopifnot(is.character(logfile))
  stopifnot(is.numeric(days), is.numeric(max_size))
  #
  # make sure that that directory exists and drag the user into that process
  cache_exists <- file.exists(cache_dir)
  cache_usable <- file.access(cache_dir, mode = 6) == 0
  if (!(cache_exists)) {
    if (confirm) {
      user_input <- readline(glue::glue(
        "there is no directory at {cache_dir}; the cache will be turned off until a ",
        "viable directory has been set. Create one? (yes|no(default)) "
      ))
      repeat {
        valid_user_input <- ifelse(grepl("yes|no", user_input), sub(".*(yes|no).*", "\\1", user_input), NA)
        if (user_input == "") {
          valid_user_input <- ""
        }
        if (!is.na(valid_user_input)) {
          break
        }
        user_input <- readline(glue::glue(" please answer either yes or no: "))
      }
    } else {
      valid_user_input <- "yes"
    }
    if (valid_user_input == "yes") {
      dir.create(cache_dir, showWarnings = TRUE, recursive = TRUE)
      cache_exists <- TRUE
      cache_usable <- file.access(cache_dir, mode = 6) == 0
    }
  }

  if (!cache_usable) {
    print(glue::glue(
      "The directory at {cache_dir} is not accessible; check permissions and/or use a different ",
      "directory for the cache (see the `set_cache` documentation)."
    ))
  } else if (cache_exists) {
    cache_environ$epidatr_cache <- cachem::cache_disk(
      dir = cache_dir,
      max_size = max_size * 1024^2,
      max_age = days * 24 * 60 * 60,
      logfile = file.path(cache_dir, logfile)
    )
    cache_environ$cache_args <- list(
      cache_dir = cache_dir,
      days = days,
      max_size = max_size,
      logfile = logfile
    )
  }

  cli::cli_inform(c(
    "!" = "epidatr cache is being used (set env var EPIDATR_USE_CACHE=FALSE if not intended).",
    "i" = "The cache directory is {cache_dir}.",
    "i" = "The cache will be cleared after {days} day{ifelse(days>1,'s','')}
           and will be pruned if it exceeds {max_size} MB.",
    "i" = "The log of cache transactions is stored at {file.path(cache_dir, logfile)}."
  ), class = if (startup) "packageStartupMessage" else NULL)
}

#' Manually reset the cache, deleting all currently saved data and starting afresh
#' @description
#' Deletes the current cache and resets a new cache. Deletes local data! If you
#'   are using a session unique cache, the previous settings will be reused. If
#'   you pass in new `set_cache` arguments, they will take precedence over the
#'   previous settings.
#' @param disable instead of setting a new cache, disable caching entirely;
#'   defaults to `FALSE`
#' @inheritDotParams set_cache
#' @return [`NULL`] no return value, all effects are stored in the package
#'         environment
#' @seealso [`set_cache`] to start a new cache (and general caching info),
#'   [`disable_cache`] to only disable without deleting, and [`cache_info`]
#' @export
#' @import cachem
#' @importFrom rlang dots_list inject
clear_cache <- function(..., disable = FALSE) {
  if (any(!is.na(cache_environ$epidatr_cache))) {
    cache_environ$epidatr_cache$destroy()
    recovered_args <- cache_environ$cache_args
    cache_environ$cache_args <- NULL
  } else {
    recovered_args <- list()
  }
  args <- dots_list(
    ...,
    confirm = FALSE,
    !!!recovered_args,
    .homonyms = "first",
    .ignore_empty = "all"
  )
  if (disable) {
    cache_environ$epidatr_cache <- NULL
  } else {
    inject(set_cache(!!!args))
  }
}

#' Turn off the caching for this session
#' @description
#' Disable caching until you call `set_cache` or restart R. The files defining
#'   the cache are untouched. If you are looking to disable the caching more
#'   permanently, set `EPIDATR_USE_CACHE=FALSE` as environmental variable in
#'   your `.Renviron`.
#' @return [`NULL`] no return value, all effects are stored in the package
#'         environment
#' @export
#' @seealso [`set_cache`] to start a new cache (and general caching info),
#'   [`clear_cache`] to delete the cache and set a new one, and [`cache_info`]
#' @import cachem
disable_cache <- function() {
  cache_environ$epidatr_cache <- NULL
}

#' Describe current cache
#'
#' @description
#' Print out the information about the cache (as would be returned by cachem's
#' `info()` method).
#' @return [`list`] containing the info result as created by cachem
#'
#' @seealso [`set_cache`] to start a new cache (and general caching info),
#'   [`clear_cache`] to delete the cache and set a new one, and [`disable_cache`] to
#'   disable without deleting
#' @export
cache_info <- function() {
  if (is_cache_enabled()) {
    return(cache_environ$epidatr_cache$info())
  } else {
    return("there is no cache")
  }
}

#' Check if the cache is enabled
#' @keywords internal
is_cache_enabled <- function() {
  !is.null(cache_environ$epidatr_cache)
}

#' Helper that checks whether a call is actually cachable
#'
#' The cacheable endpoints are those with `as_of` or `issues` parameters:
#' - pub_covidcast
#' - pub_covid_hosp_state_timeseries
#' - pub_ecdc_ili
#' - pub_flusurv
#' - pub_fluview_clinical
#' - pub_fluview
#' - pub_kcdc_ili
#' - pub_nidss_flu
#' - pub_paho_dengue
#'
#' @keywords internal
check_is_cachable <- function(epidata_call, fetch_args) {
  as_of_cachable <- !is.null(epidata_call$params$as_of) && !identical(epidata_call$params$as_of, "*")
  issues_cachable <- !is.null(epidata_call$params$issues) && !identical(epidata_call$params$issues, "*")
  is_cachable <- (
    # Cache should be enabled
    is_cache_enabled() &&
      # Call should be cachable
      (as_of_cachable || issues_cachable) &&
      # This should not be a dry run
      !fetch_args$dry_run &&
      # Base url should be null
      is.null(fetch_args$base_url) &&
      # Don't cache debug calls
      !fetch_args$debug &&
      # Format type should be json
      fetch_args$format_type == "json" &&
      # Fields should be null
      is.null(fetch_args$fields) &&
      # Disable date parsing should be false
      !fetch_args$disable_date_parsing &&
      # Disable data frame parsing should be false
      !fetch_args$disable_data_frame_parsing &&
      # Refresh cache should be false
      fetch_args$refresh_cache == FALSE
  )
  return(is_cachable)
}

#' Check for warnings for the cache
#'
#' @description
#' Adds warnings when arguments are potentially too recent to use with the cache.
#'
#' @param epidata_call the `epidata_call` object
#' @param fetch_args the args list for fetch as generated by [`fetch_args_list()`]
#' @keywords internal
check_for_cache_warnings <- function(epidata_call, fetch_args) {
  as_of_recent <- check_is_recent(epidata_call$params$as_of, 7)
  issues_recent <- check_is_recent(epidata_call$params$issues, 7)
  if (as_of_recent || issues_recent) {
    cli::cli_warn(
      c(
        "Using cached results with `as_of` within the past week (or the future!).
          This will likely result in an invalid cache. Consider",
        "i" = "disabling the cache for this session with `disable_cache` or
          permanently with environmental variable `EPIDATR_USE_CACHE=FALSE`",
        "i" = "setting `EPIDATR_CACHE_MAX_AGE_DAYS={Sys.getenv('EPIDATR_CACHE_MAX_AGE_DAYS
          ', unset = 1)}` to e.g. `3/24` (3 hours)."
      ),
      .frequency = "regularly",
      .frequency_id = "cache timing issues",
      class = "cache_recent_data"
    )
  }
}
