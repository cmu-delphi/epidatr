#' Helper function to cast values, non-list vectors, and/or EpiRanges to strings
#'
#' @noRd
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
#' @noRd
format_list <- function(values) {
  paste(vapply(values, format_item, character(1L)), collapse = ",")
}
