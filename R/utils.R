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
  (!is.null(dates) && any(dates >= format(Sys.Date() - max_age, format = "%Y%m%d")))
}

#' helper that checks whether a call is actually cachable
#'
#' @keywords internal
check_is_cachable <- function(epidata_call, fetch_args) {
  as_of_cachable <- (!is.null(epidata_call$params$as_of) && epidata_call$params$as_of != "*")
  issues_cachable <- (!is.null(epidata_call$params$issues) && all(epidata_call$params$issues != "*"))
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
