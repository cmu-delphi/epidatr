# Helper function to cast values and/or ranges to strings
format_item <- function(value) {
  if (inherits(value, "EpiRange")) {
    paste0(toString(value$from), "-", toString(value$to))
  } else if (is.list(value) &&
    "from" %in% names(value) && "to" %in% names(value) && length(names(value)) == 2) {
    paste0(toString(value$from), "-", toString(value$to))
  } else {
    paste(value, collapse = ",")
  }
}

# Helper function to build a list of values and/or ranges
format_list <- function(values) {
  if (!is.list(values) ||
    ("from" %in% names(values) && "to" %in% names(values))) {
    values <- list(values)
  }
  paste(vapply(values, format_item, character(1L)), collapse = ",")
}
