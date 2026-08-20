# Validation functions for user input.

#' Allows character vectors
#' @importFrom checkmate assert_character assert_integerish
#' @keywords internal
assert_character_param <- function(name, value, len = NULL, required = TRUE) {
  null_ok <- !required
  assert_integerish(len, null.ok = TRUE, .var.name = "len")
  assert_character(
    value,
    null.ok = null_ok,
    len = len,
    any.missing = FALSE,
    .var.name = name
  )
}

#' Allows integer-like vectors
#' @importFrom checkmate assert_integerish
#' @keywords internal
assert_integerish_param <- function(name, value, len = NULL, required = TRUE) {
  null_ok <- !required
  assert_integerish(len, null.ok = TRUE, .var.name = "len")
  assert_integerish(
    value,
    null.ok = null_ok,
    len = len,
    any.missing = FALSE,
    .var.name = name
  )
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

#' Helper to format the 'version' argument for the CAST API version_query
#' @param version the version argument containing operator and date, exact date,
#' or numeric date
#' @return a formatted version_query string (e.g., "<2025-10-16", "=2025-10-16")
#' @keywords internal
validate_version_query <- function(version) {
  if (is.null(version) || identical(version, "*")) {
    return(NULL)
  }

  operator <- "="
  if (
    is.character(version) && length(version) == 1 && grepl("^[<>=]", version)
  ) {
    operator <- substr(version, 1, 1)
    version <- substr(version, 2, nchar(version))
  } else if (inherits(version, "EpiRange")) {
    # Lower bound is filtered out locally
    operator <- "<"
    version <- version$to
  }

  # Validate and standardize the date part
  assert_date_param("version", version, len = 1L, required = FALSE)
  formatted_date <- format(parse_api_date(version), "%Y-%m-%d")

  if (is.na(formatted_date)) {
    cli::cli_abort(
      paste0(
        "Invalid `version` format. Must be a single date, an `EpiRange`, ",
        "or a character string with an operator (e.g., '<2025-10-16')."
      ),
      class = "epidatr__invalid_version_query"
    )
  }

  paste0(operator, formatted_date)
}


#' helper to convert a date wildcard ("*") to an appropriate epirange
#'
#' @keywords internal
get_wildcard_equivalent_dates <- function(
  time_value,
  time_type = c("day", "week")
) {
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
#' Check an API response for epidata-level errors and warnings.
#'
#' @param response_content parsed JSON response with `result` and `message` fields
#' @param allow_empty if TRUE, suppress errors for "no results" (result == -2)
#' @importFrom cli cli_abort cli_warn
#' @keywords internal
check_epidata_result <- function(response_content, allow_empty = FALSE) {
  # success is 1, no results is -2, truncated is 2, -1 is generic error
  if (response_content$result != 1) {
    if ((response_content$result != -2) && !allow_empty) {
      cli::cli_abort(
        "epidata error: {.code {response_content$message}}",
        class = "epidata_error"
      )
    }
  }

  if (response_content$message != "success") {
    cli::cli_warn(
      "epidata warning: {.code {response_content$message}}",
      class = "epidata_warning"
    )
  }
}
