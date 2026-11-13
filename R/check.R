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
    cli::cli_abort(
      c(
        "{.arg {name}} must have length {len}.",
        "x" = "It has length {length(value)}."
      ),
      class = "epidatr__bad_length"
    )
  }
}

#' Format a report-time-family value the way the cast-API accepts it.
#'
#' Aborts on a value that can't be formatted (such as a timestamp without a
#' `Z`, or `"latest"`), rather than sending `NA` to the API.
#' @param name argument name for error messages.
#' @keywords internal
format_report_time_bound <- function(value, name = "snapshot_date") {
  if (is.null(value)) {
    return(NULL)
  }
  utc_timestamp <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}(:\\d{2}(\\.\\d{1,6})?)?Z$"
  if (is.character(value) && length(value) == 1 && grepl(utc_timestamp, value)) {
    return(value)
  }
  out <- if (is.character(value) && any(grepl("T", value, fixed = TRUE))) {
    # A timestamp that isn't UTC; as.Date() would silently keep only its date.
    NA_character_
  } else if (inherits(value, "POSIXt")) {
    format(as.POSIXct(value, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  } else {
    format(parse_api_date(value), "%Y-%m-%d")
  }
  if (anyNA(out)) {
    cli::cli_abort(
      c(
        "{.arg {name}} must be a date or a UTC timestamp, not {.val {as.character(value)}}.",
        "i" = "Use a date like {.val 2025-01-15} or a UTC timestamp like
          {.val 2025-01-15T13:45:00Z}."
      ),
      class = "epidatr__invalid_version_query"
    )
  }
  out
}

#' Abort if more than one of the named arguments is non-NULL
#'
#' Pass the arguments by name, e.g. `assert_exclusive(issues = issues, lag = lag)`.
#' @param ... named arguments; `NULL` means "not supplied".
#' @param class extra condition class to add to the error.
#' @param call the call shown in the error message.
#' @keywords internal
assert_exclusive <- function(..., class = NULL, call = rlang::caller_env()) {
  args <- list(...)
  supplied <- names(args)[!vapply(args, is.null, logical(1))]
  if (length(supplied) > 1) {
    all_names <- names(args) # nolint: object_usage_linter
    cli::cli_abort(
      c(
        "{.arg {all_names}} are mutually exclusive.",
        "x" = "You supplied {.arg {supplied}}."
      ),
      class = c(class, "epidatr__exclusive_args"),
      call = call
    )
  }
  invisible(NULL)
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
      assert_uniform_granularity(value)
      return(value)
    } else {
      cli::cli_abort(
        c(
          "Invalid timeset input.",
          "x" = "Got {.val {value}}; expected dates, YYYYMMDD or YYYYWW values, {.val *}, or an {.fn epirange}."
        ),
        class = "epidatr__invalid_timeset"
      )
    }
  } else if (test_character(value)) {
    if (identical(value, "*")) {
      return(value)
    } else if (all(nchar(value) %in% c(6, 8))) {
      assert_uniform_granularity(value)
      return(value)
    } else if (all(nchar(value) == 10)) {
      value <- as.Date(value, format = "%Y-%m-%d")
      return(format(value, format = "%Y%m%d"))
    } else {
      cli::cli_abort(
        c(
          "Invalid timeset input.",
          "x" = "Got {.val {value}}; expected dates, YYYYMMDD or YYYYWW values, {.val *}, or an {.fn epirange}."
        ),
        class = "epidatr__invalid_timeset"
      )
    }
  } else if (test_class(value, "EpiRange")) {
    return(value)
  } else {
    cli::cli_abort(
      c(
        "Invalid timeset input.",
        "x" = "Got {.val {value}}; expected dates, YYYYMMDD or YYYYWW values, {.val *}, or an {.fn epirange}."
      ),
      class = "epidatr__invalid_timeset"
    )
  }
}

#' Granularity ("day" or "week") of each value in a timeset, judged by digit count
#' @keywords internal
timeset_granularity <- function(value) {
  if (inherits(value, "EpiRange")) {
    value <- value$from
  }
  if (inherits(value, "Date")) {
    return("day")
  }
  if (identical(value, "*")) {
    return(character(0))
  }
  ifelse(nchar(value) == 6, "week", "day")
}

#' Stop if a timeset mixes weekly (YYYYWW) and daily (YYYYMMDD) values
#' @keywords internal
assert_uniform_granularity <- function(value) {
  if (length(unique(timeset_granularity(value))) > 1) {
    cli::cli_abort(
      "Timeset mixes day (YYYYMMDD) and week (YYYYWW) values; use one kind per parameter.",
      class = "epidatr__mixed_timeset"
    )
  }
}

#' Stop if timeset values are not of the kind (`"day"` or `"week"`) expected
#' @keywords internal
assert_time_type_matches <- function(name, value, time_type) {
  found <- timeset_granularity(value)
  if (any(found != time_type)) {
    cli::cli_abort(
      c(
        "{.arg {name}} expects {time_type} values ({if (time_type == 'week') 'YYYYWW' else 'YYYYMMDD'}).",
        "x" = "Got {setdiff(found, time_type)[1]} values; convert them first.",
        "i" = if (time_type == "week") "Epiweeks can be built with {.fn MMWRweek::MMWRweek}."
      ),
      class = "epidatr__timeset_granularity_mismatch"
    )
  }
}

#' Validate and parse a timeset parameter
#' @keywords internal
validate_timeset_input <- function(name, value, len = NULL, required = TRUE, granularity = c("any", "day", "week")) {
  granularity <- match.arg(granularity)
  assert_timeset_param(name, value, len = len, required = required)
  if (granularity != "any" && !is.null(value)) {
    assert_time_type_matches(name, value, granularity)
  }
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

#' Abort on epiweeks that don't exist.
#'
#' A year has 52 or 53 MMWR weeks. `MMWRweek::MMWRweek2Date()` silently wraps
#' a week past the end of the year into the next year (`202453` is 2025w01)
#' and fails cryptically on week 0 or 54+. No-op for anything that isn't a
#' YYYYWW value or an `epirange()` of them.
#' @param name argument name for error messages.
#' @param value a timeset.
#' @keywords internal
assert_valid_epiweeks <- function(name, value) {
  x <- if (inherits(value, "EpiRange")) c(value$from, value$to) else value
  if (length(x) == 0 || inherits(x, "Date") || !all(grepl("^[0-9]{6}$", x))) {
    return(invisible(value))
  }
  x <- as.integer(x)
  years <- x %/% 100L
  weeks <- x %% 100L
  last_week <- MMWRweek::MMWRweek(MMWRweek::MMWRweek2Date(years + 1L, rep(1L, length(years))) - 1L)$MMWRweek
  bad <- weeks < 1L | weeks > last_week
  if (any(bad)) {
    cli::cli_abort(
      c(
        "{.arg {name}} has an epiweek that doesn't exist: {.val {x[bad][1]}}.",
        "i" = "Year {years[bad][1]} has epiweeks 1 to {last_week[bad][1]}."
      ),
      class = "epidatr__invalid_epiweek"
    )
  }
  invisible(value)
}

# The most terms one time filter may hold. Longer lists make slow requests; a
# single range plus filtering the result locally is cheaper for everyone.
max_time_filter_terms <- 128L

#' Format a cast-API time filter (`reference_times` or `report_time_query`).
#'
#' Accepts `"*"` or `NULL` (no filter), an [epirange()], plain dates or
#' epiweeks, and filter expressions (`">=2024-01-01"`,
#' `"2024-01-01:2024-03-31"`), which pass through for the API to validate.
#' @param name argument name for error messages.
#' @param bare_dates whether plain dates are allowed. `report_time` disallows them.
#' @keywords internal
format_time_filter <- function(value, name, bare_dates = TRUE) {
  if (is.null(value) || identical(value, "*")) {
    return(NULL)
  }
  if (inherits(value, "EpiRange")) {
    assert_valid_epiweeks(name, value)
    x <- c(value$from, value$to)
    if (all(nchar(x) == 6)) {
      # Whole epiweeks: from the first week's Sunday to the last week's
      # Saturday. Weekly V5 data is dated by the week's end, and the API's
      # range end is an inclusive date.
      return(paste0(parse_api_week(x[1]), ":", parse_api_week(x[2]) + 6))
    }
    x <- parse_api_date(x)
    return(paste0(x[1], ":", x[2]))
  }
  # Expressions (">=2024-01-01", "2024-01-01:2024-03-31") pass through.
  is_expr <- is.character(value) & grepl("^[<>=]|:", value)
  out <- as.character(value)
  # Plain dates and epiweeks become one-day "d:d" ranges; the API has no "=".
  if (any(!is_expr)) {
    if (!bare_dates) {
      cli::cli_abort(
        c(
          "A bare date is not a valid {.arg {name}} value.",
          "i" = "Use a comparison like {.code \"<{value[!is_expr][1]}\"} or a range like
            {.code epirange(from, \"{value[!is_expr][1]}\")}.",
          "i" = "For data as it appeared on a specific date, use {.arg snapshot_date} instead."
        ),
        class = "epidatr__invalid_version_query"
      )
    }
    x <- validate_timeset_input(name, value[!is_expr])
    assert_valid_epiweeks(name, x)
    if (inherits(x, "Date")) {
      from <- to <- x
    } else if (all(nchar(x) == 6)) {
      # A single epiweek is the whole week, Sunday to Saturday.
      from <- parse_api_week(x)
      to <- from + 6
    } else {
      from <- to <- parse_api_date(x)
    }
    out[!is_expr] <- paste0(from, ":", to)
  }
  terms <- unique(out)
  if (length(terms) > max_time_filter_terms) {
    cli::cli_abort(
      c(
        "{.arg {name}} has {length(terms)} terms; the limit is {max_time_filter_terms}.",
        "i" = "Request one range instead, like {.code epirange(min(x), max(x))}, and filter the result locally."
      ),
      class = "epidatr__too_many_time_terms"
    )
  }
  # The API ORs the expressions.
  paste(terms, collapse = ",")
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
#' @importFrom cli cli_abort cli_warn
#' @keywords internal
check_epidata_result <- function(response_content) {
  if (!is.list(response_content)) {
    cli::cli_abort(
      "Malformed response envelope: expected a JSON object",
      class = "epidatr__malformed_envelope"
    )
  }
  result <- response_content$result
  message <- response_content$message
  if (!is.numeric(result) || length(result) != 1L || !is.character(message) || length(message) != 1L) {
    cli::cli_abort(
      "Malformed response envelope: {.field result} and {.field message} must each be a single value",
      class = "epidatr__malformed_envelope"
    )
  }
  # success is 1, no results is -2, truncated is 2, -1 is generic error
  if (result %in% c(1, 2) && is.null(response_content$epidata)) {
    cli::cli_abort(
      "Malformed response envelope: {.field result} is {result} but {.field epidata} is missing",
      class = "epidatr__malformed_envelope"
    )
  }
  # Only success and no-results (which warns below) are accepted. Truncation (2)
  # and generic errors (-1) always abort, so partial data is never returned or
  # cached, whatever `return_empty` is.
  if (!result %in% c(1, -2)) {
    cli::cli_abort(
      "epidata error: {.code {response_content$message}}",
      class = "epidata_error"
    )
  }

  if (response_content$message != "success") {
    cli::cli_warn(
      "epidata warning: {.code {response_content$message}}",
      class = "epidata_warning"
    )
  }
}
