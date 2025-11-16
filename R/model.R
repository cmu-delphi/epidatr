#' Specify a range of days or weeks for API requests
#'
#' Specify a date range (in days or epiweeks) for an API request.
#'
#' @param from The first date to request. Can be specified as a `Date` or as an
#'   integer or integer-like string in the format YYYYMMDD for dates or YYYYWW
#'   for epiweeks.
#' @param to The final date to request (inclusive), specified the same way as
#'   `from`.
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

#' helper to convert an epirange from week to day or vice versa
#'
#' @keywords internal
reformat_epirange <- function(epirange, to_type = c("day", "week")) {
  to_type <- match.arg(to_type)

  # Day format -> week
  if (nchar(epirange$from) == 8 && to_type == "week") {
    return(
      epirange(date_to_epiweek(epirange$from), date_to_epiweek(epirange$to))
    )
    # Week format -> day
  } else if (nchar(epirange$from) == 6 && to_type == "day") {
    return(
      epirange(parse_api_week(epirange$from), parse_api_week(epirange$to))
    )
  }

  return(epirange)
}

#' @export
print.EpiRange <- function(x, ...) {
  if (nchar(x$from) == 8) {
    date_type <- "Days" # nolint: object_usage_linter
    x$from <- as.Date(as.character(x$from), "%Y%m%d")
    x$to <- as.Date(as.character(x$to), "%Y%m%d")
  } else if (nchar(x$from) == 6) {
    date_type <- "Epiweeks" # nolint: object_usage_linter
    x$from <- paste0(
      substr(x$from, 1, 4), "w", substr(x$from, 5, 6)
    )
    x$to <- paste0(
      substr(x$to, 1, 4), "w", substr(x$to, 5, 6)
    )
  }

  cli::cli_h1("<EpiRange> object:")
  cli::cli_bullets(
    "{date_type} from {x$from} to {x$to}"
  )
}

#' Timeset formats for specifying dates
#'
#' Many API calls accept timesets to specify the time ranges of data being
#' requested. Timesets can be specified with `epirange()`, as `Date` objects, or
#' with wildcards.
#'
#' Timesets are not special R types; the term simply describes any value that is
#' accepted by epidatr to specify the time value of an epidata query:
#'
#' - Dates: `Date` instances.
#' - Date strings or integers: Strings or integers in the format YYYYMMDD.
#' - Epiweeks: Strings or integers in the format YYYYWW, where WW is the epiweek
#'   number.
#' - EpiRanges: A range returned by `epirange()`, or a list of multiple ranges.
#' - Wildcard: The string `"*"`, which requests all available time values.
#'
#' Refer to the specific endpoint documentation for guidance on using dates vs
#' weeks. Most endpoints support only one or the other. Some (less commonly
#' used) endpoints may not accept the `"*"` wildcard, but this can be simulated
#' with a large `epirange()`.
#'
#' @name timeset
NULL

create_epidata_field_info <- function(name,
                                      type,
                                      description = "",
                                      categories = c()) {
  checkmate::assert_character(name, len = 1)
  checkmate::assert_character(type, len = 1)
  checkmate::assert_subset(type, c(
    "text",
    "int",
    "float",
    "date",
    "epiweek",
    "categorical",
    "bool"
  ))
  checkmate::assert_character(description, len = 1)
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

#' @export
print.EpidataFieldInfo <- function(x, ...) {
  cli::cli_h1("<EpidataFieldInfo> object:")
  # Print all non-class fields.
  cli::cli_dl(x[attr(x, "names")])
}

#' @importFrom stats na.omit
parse_value <- function(info, value, disable_date_parsing = FALSE, reference_week_day = 1) {
  stopifnot(inherits(info, "EpidataFieldInfo"))

  if (is.null(value)) {
    return(value)
  } else if (info$type == "date" && !disable_date_parsing && !inherits(value, "Date")) {
    return(parse_api_date(value))
  } else if (info$type == "epiweek" && !disable_date_parsing && !inherits(value, "Date")) {
    return(parse_api_week(value, reference_week_day = reference_week_day))
  } else if (info$type == "bool") {
    return(as.logical(value))
  } else if (info$type == "int") {
    # Int doesn't have enough capacity to store some weekly `pub_wiki` values.
    value <- as.double(value)
    if (any(na.omit(value != round(value)))) {
      cli::cli_warn(
        c(
          "Values in {info$name} were expected to be integers but contain a decimal component",
          "i" = "Decimal components are returned as-is"
        ),
        class = "epidatr__int_nonzero_decimal_digits"
      )
    }

    return(value)
  } else if (info$type == "float") {
    return(as.double(value))
  } else if (info$type == "categorical") {
    return(factor(value, levels = info$categories))
  }
  value
}

#' @importFrom purrr map_chr
parse_data_frame <- function(epidata_call, df, disable_date_parsing = FALSE, reference_week_day = 1) {
  stopifnot(inherits(epidata_call, "epidata_call"))
  meta <- epidata_call$meta
  df <- as.data.frame(df)

  if (length(meta) == 0) {
    return(df)
  }

  meta_field_names <- map_chr(meta, "name")
  missing_fields <- setdiff(names(df), meta_field_names)
  if (
    length(missing_fields) != 0
  ) {
    cli::cli_warn(
      c(
        "Not all return columns are specified as expected epidata fields",
        "i" = "Unspecified fields {missing_fields} may need to be manually converted to more appropriate classes"
      ),
      class = "epidatr__missing_meta_fields"
    )
  }

  columns <- colnames(df)
  for (i in seq_len(length(meta))) {
    info <- meta[[i]]
    if (info$name %in% columns) {
      df[[info$name]] <- parse_value(
        info,
        df[[info$name]],
        disable_date_parsing = disable_date_parsing,
        reference_week_day = reference_week_day
      )
    }
  }
  df
}

#' Converts a date (integer or character) to an epiweek
#' @param value date (integer or character, with format YYYYMMDD) to be converted to an epiweek
#' @return an integer representing an epiweek, in the format YYYYWW
#' @importFrom MMWRweek MMWRweek
#' @keywords internal
date_to_epiweek <- function(value) {
  date_components <- MMWRweek::MMWRweek(parse_api_date(value))
  as.numeric(paste0(
    date_components$MMWRyear,
    # Pad with zeroes up to 2 digits (x -> 0x)
    formatC(date_components$MMWRweek, width = 2, flag = 0)
  ))
}

#' @keywords internal
parse_api_date <- function(value) {
  as.Date(as.character(value), tryFormats = c("%Y%m%d", "%Y-%m-%d"))
}

#' parse_api_week converts an integer to a date
#' @param value value to be converted to an epiweek
#' @param reference_week_day the day of the week to use as the reference day. Defaults to Sunday.
#' @return a date
#' @importFrom MMWRweek MMWRweek2Date
#' @keywords internal
parse_api_week <- function(value, reference_week_day = 1) {
  v <- as.integer(value)
  years <- floor(v / 100)
  weeks <- v - (years * 100)
  MMWRweek::MMWRweek2Date(years, weeks, MMWRday = reference_week_day)
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
