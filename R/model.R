#' EpiRange
#'
#' @description
#' Specify a date range (in days or epiweeks) for an API request.
#'
#' @param from A `Date`, integer-like value, or integer-like string that takes the
#'   form YYYYMMDD for dates or YYYYWW for epiweeks.
#' @param to A `Date`, integer-like value, or integer-like string that takes the
#'  form YYYYMMDD for dates or YYYYWW for epiweeks.
#' @return An `EpiRange` object.
#' @importFrom checkmate check_integerish check_character check_date assert
#'
#' @details
#' Epiweeks, also known as MMWR weeks number the weeks of the year from 1 to 53,
#' each week spanning from Sunday to Saturday. The numbering is [defined by the
#' CDC](https://ndc.services.cdc.gov/wp-content/uploads/MMWR_Week_overview.pdf).
#'
#' @examples
#' # Represents 2021-01-01 to 2021-01-07, inclusive
#' epirange(20210101, 20210107)
#'
#' # The same, but using Date objects
#' epirange(as.Date("2021-01-01"), as.Date("2021-01-07"))
#'
#' # Represents epiweeks 2 through 4 of 2022, inclusive
#' epirange(202202, 202204)
#' @export
epirange <- function(from, to) {
  assert(
    check_integerish(from, len = 1),
    check_character(from, len = 1),
    check_date(from, len = 1),
    .var.name = "from"
  )
  assert(
    check_integerish(to, len = 1),
    check_character(to, len = 1),
    check_date(to, len = 1),
    .var.name = "to"
  )

  from <- parse_timeset_input(from)
  to <- parse_timeset_input(to)
  if (inherits(from, "Date")) {
    from <- as.numeric(format(from, "%Y%m%d"))
  }
  if (inherits(to, "Date")) {
    to <- as.numeric(format(to, "%Y%m%d"))
  }

  if (nchar(from) != nchar(to)) {
    stop(paste0("EpiRange error, from (", from, ") and to (", to, ") must be the same format"))
  }

  if (to < from) {
    t <- from
    from <- to
    to <- t
  }

  structure(list(from = from, to = to), class = "EpiRange")
}


#' Timeset
#'
#' @description
#' Many API calls accept timesets to specify the time ranges of data being
#' requested. Timesets can be specified with `epirange()`, as `Date` objects, or
#' with wildcards.
#'
#' Timesets are not special R types; the term simply describes any value that
#' would be accepted by epidatr to specify the time value of an epidata query.
#' The allowed values are:
#'
#' - Dates: `Date` instances, integer-like values, or integer-like strings that
#'   take the form YYYYMMDD.
#' - Epiweeks: Integer-like values or integer-like strings that take the form
#'   YYYYWW.
#' - EpiRanges: A range returned by `epirange()`, or a list of multiple ranges.
#' - Wildcard: The string `"*"`, which request all available time values.
#'
#' Please refer to the specific endpoint documentation for guidance on using
#' dates vs weeks. Most endpoints support only one or the other. Some (less
#' commonly used) endpoints may not accept the `"*"` wildcard, but this can be
#' simulated with a large `epirange()`.
#'
#' @name timeset
NULL


create_epidata_field_info <- function(name,
                                      type,
                                      description = "",
                                      categories = c()) {
  stopifnot(is.character(name) && length(name) == 1)
  stopifnot(
    is.character(type) &&
      length(type) == 1 &&
      type %in% c(
        "text",
        "int",
        "float",
        "date",
        "epiweek",
        "categorical",
        "bool"
      )
  )
  stopifnot(is.character(description) && length(description) == 1)
  structure(
    list(
      name = name,
      type = type,
      description = description,
      categories = categories
    ),
    class = "EpidataFieldInfo"
  )
}

parse_value <- function(info, value, disable_date_parsing = FALSE) {
  stopifnot(inherits(info, "EpidataFieldInfo"))

  if (is.null(value)) {
    return(value)
  } else if (info$type == "date" && !disable_date_parsing) {
    return(parse_api_date(value))
  } else if (info$type == "epiweek" && !disable_date_parsing) {
    return(parse_api_week(value))
  } else if (info$type == "bool") {
    return(as.logical(value))
  } else if (info$type == "int") {
    return(as.integer(value))
  } else if (info$type == "float") {
    return(as.double(value))
  } else if (info$type == "categorical") {
    return(factor(value, levels = info$categories))
  }
  value
}

parse_data_frame <- function(epidata_call, df, disable_date_parsing = FALSE) {
  stopifnot(inherits(epidata_call, "epidata_call"))
  meta <- epidata_call$meta
  df <- as.data.frame(df)
  if (length(meta) == 0) {
    return(df)
  }
  columns <- colnames(df)
  for (i in seq_len(length(meta))) {
    info <- meta[[i]]
    if (info$name %in% columns) {
      df[[info$name]] <- parse_value(info, df[[info$name]], disable_date_parsing = disable_date_parsing)
    }
  }
  df
}

#' @keywords internal
parse_api_date <- function(value) {
  as.Date(as.character(value), format = "%Y%m%d")
}

#' parse_api_week converts an integer to a date
#' @param value value to be converted to an epiweek
#' @return a date
#' @importFrom MMWRweek MMWRweek2Date
#' @keywords internal
parse_api_week <- function(value) {
  v <- as.integer(value)
  years <- floor(v / 100)
  weeks <- v - (years * 100)
  MMWRweek::MMWRweek2Date(years, weeks)
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
