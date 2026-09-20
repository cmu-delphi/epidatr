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

#' Allows the cast-API `limit` param: NULL (no limit), -1 (no limit), or a
#' positive integer
#' @keywords internal
assert_limit_param <- function(value) {
  assert_integerish_param("limit", value, len = 1, required = FALSE)
  if (!is.null(value) && value != -1 && value < 1) {
    cli::cli_abort(
      "{.arg limit} must be -1 (no limit) or a positive integer",
      class = "epidatr__invalid_limit"
    )
  }
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

#' Allows a report-time-family param (a `report_time` comparison bound or
#' `snapshot_date`): date, character, integer-like, or `POSIXt`
#' @importFrom checkmate check_class
#' @keywords internal
assert_report_time_param <- function(name, value, len = NULL, required = TRUE) {
  null_ok <- !required
  assert_integerish(len, null.ok = TRUE, .var.name = "len")
  assert(
    check_date(value, len = len, any.missing = FALSE, null.ok = null_ok),
    check_character(value, len = len, any.missing = FALSE, null.ok = null_ok),
    check_integerish(value, len = len, any.missing = FALSE, null.ok = null_ok),
    check_class(value, "POSIXt", null.ok = null_ok),
    combine = "or",
    .var.name = name
  )
  if (!is.null(len) && inherits(value, "POSIXt") && length(value) != len) {
    stop(sprintf("Assertion on '%s' failed: Must have length %d, but has length %d.", name, len, length(value)))
  }
}

#' Format a report-time-family value the way the cast-API accepts it.
#' @keywords internal
format_report_time_bound <- function(value) {
  utc_timestamp <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}(:\\d{2}(\\.\\d{1,6})?)?Z$"
  if (is.character(value) && length(value) == 1) {
    if (grepl(utc_timestamp, value)) {
      return(value)
    }
    if (grepl("T", value, fixed = TRUE)) {
      return(NA_character_)
    }
  }
  if (inherits(value, "POSIXt")) {
    return(format(as.POSIXct(value, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  }
  format(parse_api_date(value), "%Y-%m-%d")
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

#' Helper to format the 'version' argument for the CAST API version_query.
#'
#' @param version A comparison string (e.g. `"<2025-10-16"`, `">=2025-10-16"`,
#'   or `"<=2025-10-16T13:45:00Z"` for a UTC timestamp bound) or an
#'   [`epirange()`] (dates only).
#' @return A formatted `report_time_query` string: a comparison like
#'   `"<2025-10-16"` or `"<=2025-10-16T13:45:00Z"`, or an inclusive range like
#'   `"2024-01-01:2024-03-31"`.
#' @keywords internal
validate_version_query <- function(version) {
  if (is.null(version) || identical(version, "*")) {
    return(NULL)
  }

  if (inherits(version, "EpiRange")) {
    assert_date_param("version$from", version$from, len = 1L, required = TRUE)
    assert_date_param("version$to", version$to, len = 1L, required = TRUE)
    from_date <- format(parse_api_date(version$from), "%Y-%m-%d")
    to_date <- format(parse_api_date(version$to), "%Y-%m-%d")
    return(paste0(from_date, ":", to_date))
  }

  operator <- NULL
  if (is.character(version) && length(version) == 1 && grepl("^(<=?|>=?|=)", version)) {
    op_match <- regmatches(version, regexpr("^(<=?|>=?|=)", version))
    operator <- op_match
    version <- substr(version, nchar(op_match) + 1L, nchar(version))
  }

  if (is.null(operator)) {
    cli::cli_abort(
      c(
        "A bare date is not a valid {.arg report_time} value.",
        "i" = "Use a comparison like {.code \"<{version}\"} or a range like
          {.code epirange(from, \"{version}\")}.",
        "i" = "For data as it appeared on a specific date, use {.arg snapshot_date} instead."
      ),
      class = "epidatr__invalid_version_query"
    )
  }

  if (operator == "=") {
    cli::cli_abort(
      c(
        "The {.code =} operator is not supported for {.arg report_time}.",
        "i" = "Use a comparison like {.code \"<{version}\"} or a range like
          {.code epirange(from, \"{version}\")}.",
        "i" = "For data as it appeared on a specific date, use {.arg snapshot_date} instead."
      ),
      class = "epidatr__invalid_version_query"
    )
  }

  assert_report_time_param("version", version, len = 1L, required = FALSE)
  formatted_bound <- format_report_time_bound(version)

  if (is.na(formatted_bound)) {
    cli::cli_abort(
      paste0(
        "Invalid `version` format. Must be a comparison string with an operator ",
        "(e.g., '<2025-10-16', '>=2025-10-16', or '<=2025-10-16T13:45:00Z') or an `epirange()`."
      ),
      class = "epidatr__invalid_version_query"
    )
  }

  paste0(operator, formatted_bound)
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
