# Functions relating output caching for API calls.

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
#'   `EPIDATR_CACHE_DIR="/your/directory/here"` in your `.Renviron`, for
#'   example by calling `file.edit("~/.Renviron")`.
#' See the parameters below for some more configurables if you're so inclined.
#'
#' The environmental variables, and the `set_cache()` argument each one is the
#'   default for, are:
#'
#' | Variable | Default | `set_cache()` argument |
#' |---|---|---|
#' | `EPIDATR_USE_CACHE` | unset (cache off) | none; `TRUE` enables the cache at startup |
#' | `EPIDATR_CACHE_DIR` | `rappdirs::user_cache_dir("R", version = "epidatr")` | `cache_dir` |
#' | `EPIDATR_CACHE_MAX_AGE_DAYS` | `1` | `days` |
#' | `EPIDATR_CACHE_MAX_SIZE_MB` | `1024` | `max_size` |
#' | `EPIDATR_CACHE_LOGFILE` | `"logfile.txt"` | `logfile` |
#'
#' `set_cache` (re)defines the cache to use in a particular R session. This does
#'   not clear existing data at any previous location, but instead creates a
#'   handle to the new cache using [cachem](https://cachem.r-lib.org/index.html)
#'   that seamlessly handles caching for you.
#' Say your cache is normally stored in some default directory, but for the
#'   current session you want to save your results in
#'   `~/my/temporary/savedirectory`, then you would call
#'   `set_cache(cache_dir = "~/my/temporary/savedirectory")`.
#' Or if you know the data from 2 days ago is wrong, you could call
#'   `set_cache(days = 1)` to clear older data whenever the cache is referenced.
#' In both cases, these changes would only last for a single session (though the
#'   deleted data would be gone permanently!).
#'
#' An important feature of the caching in this package is that only calls for
#'   a fixed past version of the data will actually cache. For V5 endpoints, that
#'   means `epidata_snapshot()` with a `snapshot_date` at least two days ago
#'   (UTC), or `epidata_archive()` with a `report_time` range (or `"<date"`)
#'   ending at least two days ago; a "latest" snapshot or an open-ended archive
#'   won't cache. The extra day allows for data loaded after its nominal
#'   release time. For legacy
#'   endpoints, it means specifying either `issues` before a certain date, or
#'   `as_of` before a certain date. For example the call
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
#'   call. It stores the response before parsing, so `fetch_args` that only
#'   change parsing (such as `reference_week_day` or `disable_date_parsing`)
#'   share one cache entry. Calls with `disable_data_frame_parsing = TRUE` and
#'   the nested-list `"json"` endpoints are never cached.
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
#' @param confirm whether to ask before creating a missing `cache_dir`. Default
#'   is `TRUE`. In non-interactive sessions there is no one to ask, so a missing
#'   directory is not created and the cache stays off; set `confirm = FALSE` to
#'   create it. When the cache is enabled at startup via `EPIDATR_USE_CACHE`, a
#'   missing directory is created without asking.
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
set_cache <- function(
  cache_dir = NULL,
  days = NULL,
  max_size = NULL,
  logfile = NULL,
  confirm = TRUE,
  startup = FALSE
) {
  if (is.null(cache_dir)) {
    cache_dir <- Sys.getenv(
      "EPIDATR_CACHE_DIR",
      unset = rappdirs::user_cache_dir("R", version = "epidatr")
    )
  } else if (is.null(cache_dir)) {
    # earlier version, so no tools
    cache_dir <- Sys.getenv("EPIDATR_CACHE_DIR")
    if (cache_dir == "") {
      cli::cli_abort(
        "no valid EPIDATR_CACHE_DIR",
        class = "epidatr_cache_error"
      )
    }
  }
  stopifnot(is.character(cache_dir))
  if (is.null(days)) {
    days <- Sys.getenv("EPIDATR_CACHE_MAX_AGE_DAYS", unset = "1")
  }
  days <- as_cache_setting(days, "days", "EPIDATR_CACHE_MAX_AGE_DAYS")
  if (is.null(max_size)) {
    max_size <- Sys.getenv("EPIDATR_CACHE_MAX_SIZE_MB", unset = "1024")
  }
  max_size <- as_cache_setting(max_size, "max_size", "EPIDATR_CACHE_MAX_SIZE_MB")
  if (is.null(logfile)) {
    logfile <- Sys.getenv("EPIDATR_CACHE_LOGFILE", unset = "logfile.txt")
  }
  stopifnot(is.character(logfile))
  #
  # make sure that that directory exists and drag the user into that process
  cache_exists <- file.exists(cache_dir)
  cache_usable <- file.access(cache_dir, mode = 6) == 0
  if (!(cache_exists)) {
    if (confirm && !rlang::is_interactive()) {
      # readline() can't prompt here; it would return "" and silently decline
      valid_user_input <- "no"
    } else if (confirm) {
      user_input <- readline(glue::glue(
        "there is no directory at {cache_dir}; the cache will be turned off until a ",
        "viable directory has been set. Create one? (yes|no(default)) "
      ))
      repeat {
        valid_user_input <- ifelse(
          grepl("yes|no", user_input),
          sub(".*(yes|no).*", "\\1", user_input),
          NA
        )
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
    cli::cli_inform(
      c(
        "!" = "Could not set the epidatr cache: {cache_dir} does not exist or is not
          accessible.",
        "i" = if (!cache_exists) {
          "Create it, or call {.code set_cache(confirm = FALSE)} to create it without asking."
        } else {
          "Check its permissions, or use a different directory (see {.fn set_cache})."
        }
      ),
      class = if (startup) "packageStartupMessage" else NULL
    )
    return(invisible(NULL))
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

  cli::cli_inform(
    c(
      "!" = "epidatr cache is being used (set env var EPIDATR_USE_CACHE=FALSE if not intended).",
      "i" = "Directory: {cache_dir}; entries expire after {days} day{ifelse(days>1,'s','')}
           and the cache is pruned above {max_size} MB. Log: {file.path(cache_dir, logfile)}"
    ),
    class = if (startup) "packageStartupMessage" else NULL
  )
}

#' Validate a numeric cache setting
#'
#' Coerces `value` (an argument or environment variable string) to a single
#' positive number, aborting with an actionable message otherwise. A bare
#' `as.numeric()` turns values like `"3/24"` into `NA`, which passes
#' `is.numeric()` and later breaks every cache read inside cachem.
#' @keywords internal
as_cache_setting <- function(value, arg, env_var) {
  num <- suppressWarnings(as.numeric(value))
  if (length(num) != 1 || is.na(num) || !is.finite(num) || num <= 0) {
    cli::cli_abort(
      c(
        "Cache setting {.arg {arg}} must be a single positive number, not {.val {value}}.",
        "i" = paste0(
          "Set it with the {.arg {arg}} argument or the ",
          "{.envvar {env_var}} environment variable."
        ),
        "i" = if (arg == "days") "Fractions are fine (e.g. {.code 0.125} days is 3 hours)."
      ),
      class = "epidatr__invalid_cache_setting"
    )
  }
  num
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
#' Report where the cache lives and how big it is. The result includes cachem's
#'   `info()` fields (such as `dir`, `max_size` and `max_age`) plus `enabled`,
#'   `n_entries` and `size_mb`. Entries written by older epidatr versions share
#'   the directory and are counted; use [`clear_cache`] to remove them. When the
#'   cache is off, the directory it would use is inspected instead.
#' @return a list of class `epidatr_cache_info`; printing it also shows how to
#'   clear the cache
#'
#' @seealso [`set_cache`] to start a new cache (and general caching info),
#'   [`clear_cache`] to delete the cache and set a new one, and [`disable_cache`] to
#'   disable without deleting
#' @export
cache_info <- function() {
  enabled <- is_cache_enabled()
  if (enabled) {
    info <- cache_environ$epidatr_cache$info()
  } else {
    info <- list(
      dir = Sys.getenv(
        "EPIDATR_CACHE_DIR",
        unset = rappdirs::user_cache_dir("R", version = "epidatr")
      )
    )
  }
  files <- if (dir.exists(info$dir)) {
    list.files(info$dir, full.names = TRUE)
  } else {
    character()
  }
  structure(
    c(
      list(enabled = enabled),
      info,
      list(
        n_entries = sum(grepl("\\.rds$", files)),
        size_mb = sum(file.size(files), na.rm = TRUE) / 1024^2
      )
    ),
    class = c("epidatr_cache_info", "list")
  )
}

#' @export
print.epidatr_cache_info <- function(x, ...) {
  cli::cli_inform(c(
    "i" = "epidatr cache is {if (x$enabled) 'on' else 'off'}.",
    "i" = "Directory: {x$dir}",
    "i" = "{x$n_entries} entr{?y/ies}, {round(x$size_mb, 1)} MB (includes entries from older epidatr versions).",
    "i" = "Run {.code clear_cache()} to delete them all."
  ))
  invisible(x)
}

#' Check if the cache is enabled
#' @keywords internal
is_cache_enabled <- function() {
  !is.null(cache_environ$epidatr_cache)
}

#' helper that checks whether a call is a somewhat dangerous cache
#'
#' @keywords internal
check_is_recent <- function(dates, max_age) {
  if (inherits(dates, "Date")) {
    threshold <- Sys.Date() - max_age
  } else if (is.numeric(dates) && all(nchar(dates) == 6)) {
    # Convert threshold to epiweek
    threshold <- date_to_epiweek(Sys.Date() - max_age)
  } else if (is.numeric(dates)) {
    # If inputs are numerics, compare as YYYYMMDD
    threshold <- as.numeric(format(Sys.Date() - max_age, format = "%Y%m%d"))
  } else {
    threshold <- format(Sys.Date() - max_age, format = "%Y%m%d")
  }
  (!is.null(dates) && any(dates >= threshold))
}

#' Helper that checks whether a call is actually cachable
#'
#' A call is cachable when it asks for data as of a fixed point in the past,
#' so later reports can't change the result.
#'
#' V3/V4 endpoints with an `as_of` or `issues` parameter (other than `"*"`):
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
#' V5 endpoints, when the version is bounded by a date at least two days ago
#' (UTC):
#' - `epidata_snapshot()` and `epidata_aux()` with `snapshot_date`
#' - `epidata_archive()` and `epidata_aux()` with `report_time` given as an
#'   `epirange()` or `"<date"` (not `">date"`, `"*"`, or omitted, which are
#'   open-ended)
#'
#' V5 calls with a row `limit` are never cached, since the rows returned are
#' not stable.
#' @keywords internal
check_is_cachable <- function(epidata_call, fetch_args) {
  params <- httr2::url_parse(epidata_call$request$url)$query
  as_of_cachable <- !is.null(params$as_of) && !identical(params$as_of, "*")
  issues_cachable <- !is.null(params$issues) && !identical(params$issues, "*")
  is_cachable <- (
    # Cache should be enabled
    is_cache_enabled() &&
      # Call should be cachable
      (as_of_cachable || issues_cachable || check_is_cachable_v5(params)) &&
      # This should not be a dry run
      !fetch_args$dry_run &&
      # Base url should be null
      is.null(fetch_args$base_url) &&
      # Nested-list responses can't be cached as a data frame
      epidata_call$response_format != "json" &&
      !fetch_args$disable_data_frame_parsing &&
      # Refresh cache should be false
      fetch_args$refresh_cache == FALSE
  )
  return(is_cachable)
}

#' Latest date mentioned in versioning query parameters
#'
#' Extracts every date in `values` (`YYYY-MM-DD`, `YYYYMMDD`, or `YYYYWW`
#' epiweeks, alone, inside ranges like `20220101-20220105`, or at the start of
#' timestamps like `2025-10-16T13:45:00Z`) and returns the
#' latest as a `Date`, or `NULL` if there are none.
#' @keywords internal
latest_version_date <- function(values) {
  tokens <- version_date_tokens(values)
  if (length(tokens) == 0) {
    return(NULL)
  }
  # A token that isn't a real date (e.g. "2024-02-30" or week 99) is ignored
  # rather than raising an error here, before the request is even sent.
  dates <- do.call(c, lapply(tokens, parse_version_token))
  if (all(is.na(dates))) {
    return(NULL)
  }
  max(dates, na.rm = TRUE)
}

#' Date-like tokens in versioning query parameters
#' @keywords internal
version_date_tokens <- function(values) {
  unlist(regmatches(
    values,
    # Digit lookarounds rather than \\b, so "2025-10-16T13:45:00Z" matches;
    # digits after "." are fractional seconds, not an epiweek.
    gregexpr("(?<![\\d.])(\\d{4}-\\d{2}-\\d{2}|\\d{8}|\\d{6})(?!\\d)", values, perl = TRUE)
  ))
}

#' Parse one date token from [latest_version_date()], or `NA` if invalid
#' @keywords internal
parse_version_token <- function(token) {
  tryCatch(
    if (grepl("-", token, fixed = TRUE)) {
      as.Date(token, format = "%Y-%m-%d")
    } else if (nchar(token) == 8) {
      as.Date(token, format = "%Y%m%d")
    } else {
      # last day of the epiweek
      parse_api_week(token, reference_week_day = 7)
    },
    error = function(e) as.Date(NA)
  )
}

#' V5 part of [check_is_cachable()]
#'
#' @param params the parsed query parameters of the request URL
#' @keywords internal
check_is_cachable_v5 <- function(params) {
  # A limited query returns an unstable subset of rows.
  if (!is.null(params$limit) && !identical(params$limit, "-1")) {
    return(FALSE)
  }
  if (!is.null(params$snapshot_date)) {
    bound_date <- latest_version_date(params$snapshot_date)
  } else if (!is.null(params$report_time_query)) {
    # Comma-separated conditions are OR'd by the API, so every one of them
    # needs a fixed upper bound: "<date" or a "start:end" range.
    parts <- strsplit(params$report_time_query, ",", fixed = TRUE)[[1]]
    bounded <- vapply(parts, function(part) {
      dates <- lapply(version_date_tokens(part), parse_version_token)
      n_dates <- sum(!is.na(do.call(c, c(list(as.Date(NA)), dates))))
      if (startsWith(part, "<")) n_dates >= 1 else !startsWith(part, ">") && n_dates >= 2
    }, logical(1))
    if (length(parts) == 0 || !all(bounded)) {
      return(FALSE)
    }
    bound_date <- latest_version_date(params$report_time_query)
  } else {
    return(FALSE)
  }
  # The API's dates are UTC, and a version can still change until its UTC day
  # is over. Allow one more day, since some sources stamp a nominal release
  # time and a late load could still add rows dated that day.
  !is.null(bound_date) && !is.na(bound_date) && bound_date < utc_today() - 1
}

#' Today's date in UTC, the time zone of the API's versions
#' @keywords internal
utc_today <- function() {
  as.Date(Sys.time(), tz = "UTC")
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
  # epidata_call has no `params` field; read the versioning parameters from the
  # request URL, as check_is_cachable() does (#385).
  params <- httr2::url_parse(epidata_call$request$url)$query
  version_params <- c("as_of", "issues", "snapshot_date", "report_time_query")
  latest <- latest_version_date(unlist(params[intersect(names(params), version_params)]))
  if (!is.null(latest) && latest >= utc_today() - 7) {
    cli::cli_warn(
      c(
        "Using cached results for a data version within the past week (or the
          future!). Newer reports may change these values, so the cache may be stale. Consider",
        "*" = "disabling the cache for this session with `disable_cache()` or
          permanently with the environment variable `EPIDATR_USE_CACHE=FALSE`, or",
        "*" = "setting `EPIDATR_CACHE_MAX_AGE_DAYS` to a smaller value, e.g. `0.125`
          (3 hours)."
      ),
      .frequency = "regularly",
      .frequency_id = "cache timing issues",
      class = "cache_recent_data"
    )
  }
}
