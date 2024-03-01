#' Helper function to cast values, non-list vectors, and/or EpiRanges to strings
#'
#' @keywords internal
format_item <- function(value) {
  if (inherits(value, "EpiRange")) {
    paste0(toString(value$from), "-", toString(value$to))
  } else if (inherits(value, "Date")) {
    paste(format(value, "%Y%m%d"), collapse = ",")
  } else {
    paste(value, collapse = ",")
  }
}

#' Helper function to build a list of values and/or ranges
#'
#' @keywords internal
format_list <- function(values) {
  paste(vapply(values, format_item, character(1L)), collapse = ",")
}

#' helper that checks whether a call is a somewhat dangerous cache
#'
#' @keywords internal
check_is_recent <- function(dates, max_age) {
  if (inherits(dates, "Date")) {
    threshold <- Sys.Date() - max_age
  } else {
    threshold <- format(Sys.Date() - max_age, format = "%Y%m%d")
  }
  (!is.null(dates) && any(dates >= threshold))
}

#' helper that checks whether a call is actually cachable
#'
#' @keywords internal
check_is_cachable <- function(epidata_call, fetch_args) {
  as_of_cachable <- (!is.null(epidata_call$params$as_of) && !identical(epidata_call$params$as_of, "*"))
  issues_cachable <- (!is.null(epidata_call$params$issues) && all(!identical(epidata_call$params$issues, "*")))
  is_cachable <- (
    !is.null(cache_environ$epidatr_cache) &&
      (as_of_cachable || issues_cachable) &&
      !(fetch_args$dry_run) &&
      is.null(fetch_args$base_url) &&
      !fetch_args$debug &&
      fetch_args$format_type == "json" &&
      is.null(fetch_args$fields) &&
      !fetch_args$disable_date_parsing &&
      !fetch_args$disable_data_frame_parsing
  )
  return(is_cachable)
}

#' helper to convert a date wildcard ("*") to an appropriate epirange
#'
#' @keywords internal
get_wildcard_equivalent_dates <- function(time_value, time_type = c("day", "week")) {
  time_type <- match.arg(time_type)

  if (identical(time_value, "*")) {
    if (time_type == "day") {
      # To get all dates, set start and end dates to extreme values.
      time_value <- epirange(10000101, 30000101)
    } else if (time_type == "week") {
      time_value <- epirange(100001, 300001)
    }
  }
  return(time_value)
}

#' inserts each string as a bullet at the end of the "Prepare for release" section
#' @keywords internal
release_bullets <- function() {
  c(
    "merge to main",
    "don't use_version('patch') in the next section",
    "`use_version('patch')` is redundant because we do this in PRs",
    "`use_dev_version` is also redundant."
  )
}
