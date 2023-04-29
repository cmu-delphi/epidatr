#' Helper function to cast values and/or ranges to strings
#' @importFrom lubridate is.Date
format_item <- function(value) {
  if (inherits(value, "EpiRange")) {
    paste0(toString(value$from), "-", toString(value$to))
  } else if (is.Date(value)) {
    format(value, "%Y%m%d")
  } else {
    paste(value, collapse = ",")
  }
}

# Helper function to build a list of values and/or ranges
format_list <- function(values) {
  paste(vapply(values, format_item, character(1L)), collapse = ",")
}
