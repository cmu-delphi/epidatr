#' builds a new EpiRange instances
#'
#' @param from A Date, integer-like value, or integer-like string that takes the form YYYYMMDD for dates
#'   or YYYYMM for epiweeks.
#' @param to A Date, integer-like value, or integer-like string that takes the form YYYYMMDD for dates
#'  or YYYYMM for epiweeks.
#' @return EpiRange instance
#' @importFrom checkmate check_integerish check_character check_date assert
#'
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


#' timeset
#'
#' A collection of date-like values, including dates, epiweeks, and ranges of dates or epiweeks
#' ([`epirange`]). This is often used to specify the time dimension for an epidata query. The
#' allowed values are:
#'
#' - Dates: `Date` instances, integer-like values, or integer-like strings that take the form
#'   YYYYMMDD,
#' - Epiweeks: integer-like values or integer-like strings that take the form YYYYMM,
#' - EpiRanges: a list of [`epirange`] instances.
#'
#' Please refer to the specific endpoint documentation for guidance on using dates vs weeks. Most
#' endpoints support only one or the other.
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
  }
  if (info$type == "date" && !disable_date_parsing) {
    return(parse_api_date(value))
  }
  if (info$type == "epiweek" && !disable_date_parsing) {
    return(parse_api_week(value))
  }
  if (info$type == "bool") {
    return(as.logical(value))
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

parse_api_date <- function(value) {
  as.Date(as.character(value), format = "%Y%m%d")
}

#' parses a week
#' @param value value to be converted to an epiweek
#' @importFrom MMWRweek MMWRweek2Date
#' @return a date
parse_api_week <- function(value) {
  v <- as.integer(value)
  years <- floor(v / 100)
  weeks <- v - (years * 100)
  MMWRweek::MMWRweek2Date(years, weeks)
}

fields_to_predicate <- function(fields = NULL) {
  if (is.null(fields)) {
    return(function(x) {
      TRUE
    })
  }
  to_include <- c()
  to_exclude <- c()
  for (f in fields) {
    if (substr(f, 1, 2) == "-") {
      to_exclude <- c(to_exclude, substr(f, 2, length(f)))
    } else {
      to_include <- c(to_include, f)
    }
  }
  function(x) {
    !(x %in% to_exclude) && (length(to_include) == 0 || x %in% to_include)
  }
}

#' @importFrom checkmate test_character test_class test_date test_integerish test_list
#' @export
parse_timeset_input <- function(value) {
  if (is.null(value)) {
    return(NULL)
  } else if (test_date(value)) {
    return(value)
  } else if (test_integerish(value)) {
    if (all(nchar(value) %in% c(6, 8))) {
      return(value)
    } else {
      stop(paste0("Invalid date input: ", value))
    }
  } else if (test_character(value)) {
    if (all(nchar(value) %in% c(6, 8))) {
      return(value)
    } else if (all(nchar(value) == 10)) {
      value <- as.Date(value, format = "%Y-%m-%d")
      return(format(value, format = "%Y%m%d"))
    } else {
      stop(paste0("Invalid date input: ", value))
    }
  } else if (test_class(value, "EpiRange") || test_list(value, types = "EpiRange")) {
    return(value)
  } else {
    stop(paste0("Invalid date input: ", value))
  }
}
