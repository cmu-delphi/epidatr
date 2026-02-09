#' Allows character vectors
#' @importFrom checkmate assert_character assert_integerish
#' @keywords internal
assert_character_param <- function(name, value, len = NULL, required = TRUE) {
  null_ok <- !required
  assert_integerish(len, null.ok = TRUE, .var.name = "len")
  assert_character(value, null.ok = null_ok, len = len, any.missing = FALSE, .var.name = name)
}

#' Allows integer-like vectors
#' @importFrom checkmate assert_integerish
#' @keywords internal
assert_integerish_param <- function(name, value, len = NULL, required = TRUE) {
  null_ok <- !required
  assert_integerish(len, null.ok = TRUE, .var.name = "len")
  assert_integerish(value, null.ok = null_ok, len = len, any.missing = FALSE, .var.name = name)
}

#' Allows a vector of date_like params: date, character, or integer-like
#' @importFrom checkmate check_date check_character check_integerish
#' @keywords internal
assert_date_param <- function(name, value, len = NULL, required = TRUE) {
  null_ok <- !required
  assert_integerish(len, null.ok = TRUE, .var.name = "len")
  assert(
    check_date(value, len = len, any.missing = FALSE, null.ok = null_ok),
    check_character(value, len = len, any.missing = FALSE, null.ok = null_ok),
    check_integerish(value, len = len, any.missing = FALSE, null.ok = null_ok),
    combine = "or",
    .var.name = name
  )
}

#' Allows a timeset param: a date vector, a character vector, an integer-like
#' vector, or a single EpiRange
#' @importFrom checkmate assert check_character check_date check_integerish check_class check_list check_names
#' @keywords internal
assert_timeset_param <- function(name, value, len = NULL, required = TRUE) {
  null_ok <- !required
  assert_integerish(len, len = 1L, null.ok = TRUE, .var.name = "len")
  assert(
    check_class(value, "EpiRange", null.ok = null_ok),
    check_names(names(value), type = "unnamed"),
    combine = "or",
    .var.name = name
  )
  assert(
    check_date(value, len = len, any.missing = FALSE, null.ok = null_ok),
    check_character(value, len = len, any.missing = FALSE, null.ok = null_ok),
    check_integerish(value, len = len, any.missing = FALSE, null.ok = null_ok),
    check_class(value, "EpiRange", null.ok = null_ok),
    combine = "or",
    .var.name = name
  )
}

#' @importFrom checkmate test_character test_class test_date test_integerish test_list
#' @keywords internal
parse_timeset_input <- function(value) {
  if (is.null(value)) {
    return(NULL)
  } else if (test_date(value)) {
    return(value)
  } else if (test_integerish(value)) {
    if (all(nchar(value) %in% c(6, 8))) {
      return(value)
    } else {
      stop(paste0("Invalid timeset input: ", value))
    }
  } else if (test_character(value)) {
    if (identical(value, "*")) {
      return(value)
    } else if (all(nchar(value) %in% c(6, 8))) {
      return(value)
    } else if (all(nchar(value) == 10)) {
      value <- as.Date(value, format = "%Y-%m-%d")
      return(format(value, format = "%Y%m%d"))
    } else {
      stop(paste0("Invalid timeset input: ", value))
    }
  } else if (test_class(value, "EpiRange")) {
    return(value)
  } else {
    stop(paste0("Invalid timeset input: ", value))
  }
}

#' Validate and parse a timeset parameter
#' @keywords internal
validate_timeset_input <- function(name, value, len = NULL, required = TRUE) {
  assert_timeset_param(name, value, len = len, required = required)
  parse_timeset_input(value)
}

#' Validate and parse a date parameter
#' @keywords internal
validate_date_input <- function(name, value, len = NULL, required = TRUE) {
  assert_date_param(name, value, len = len, required = required)
  parse_timeset_input(value)
}

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

#' @importFrom checkmate test_class test_list
#' @keywords internal
format_params_for_api <- function(params) {
  # Remove NULL components
  params <- params[!vapply(params, is.null, logical(1))]

  lapply(params, function(v) {
    if (test_class(v, "EpiRange")) {
      format_item(v)
    } else if (test_list(v)) {
      format_list(v)
    } else {
      format_item(v)
    }
  })
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
