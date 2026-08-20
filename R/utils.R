# Miscellaneous helper functions that don't fit into the other files.

#' inserts each string as a bullet at the end of the "Prepare for release" section
#' @keywords internal
release_bullets <- function() {
  c(
    "merge to main",
    "don't use_version('patch') in the next section",
    "`use_version('patch')` is redundant because we do this in PRs",
    "`use_dev_version` is also redundant."
  )
}

#' List all available Epidata API endpoints
#'
#' @description
#' Fetches a data frame of all Epidata API endpoints that can be accessed using
#' this package, with a brief description.
#'
#' @return A [`tibble::tibble`] of endpoints, with two columns:
#'   \item{Endpoint}{Name of the function for accessing this API endpoint.}
#'   \item{Description}{One-sentence description of the data available at the
#'   endpoint.}
#' @export
#' @importFrom utils help.search
#'
#' @examples
#' avail_endpoints()
avail_endpoints <- function() {
  h <- help.search("endpoint",
    package = "epidatr", fields = "concept",
    agrep = FALSE
  )$matches
  tib <- tibble::tibble( # printing is much nicer than data.frame
    Endpoint = paste0(h$Name, "()"),
    Description = h$Title
  )
  cli::cli_inform(c("i" = "Data is available for the US only, unless otherwise specified"))
  tib %>% print(n = 50)
}

#' Filter a data frame by a timeset (vector of dates/epiweeks or EpiRange)
#' @param df data frame to filter
#' @param column name of the column containing time values
#' @param timeset the timeset to filter by
#' @keywords internal
filter_by_timeset <- function(df, column, timeset) {
  if (identical(timeset, "*")) {
    return(df)
  }

  values <- df[[column]]

  if (inherits(timeset, "EpiRange")) {
    from <- timeset$from
    to <- timeset$to

    if (inherits(values, "Date")) {
      if (all(nchar(from) == 8)) {
        from <- parse_api_date(from)
        to <- parse_api_date(to)
      } else if (all(nchar(from) == 6)) {
        from <- parse_api_week(from)
        to <- parse_api_week(to)
      }
    }
    mask <- values >= from & values <= to
  } else {
    if (inherits(values, "Date") && !inherits(timeset, "Date")) {
      # Handle cases where timeset is a vector of integers or strings
      if (all(nchar(timeset) == 8)) {
        timeset <- parse_api_date(timeset)
      } else if (all(nchar(timeset) == 6)) {
        timeset <- parse_api_week(timeset)
      }
    }
    mask <- values %in% timeset
  }
  df[mask, ]
}

#' Serialize named key filters into the cast-API `key:value` term string.
#'
#' The backend takes multiple filters per key as repeated `key:value` terms in a
#' single query param, so `list(pcr_target = c("a", "b"), geo_value = "ca")`
#' becomes `"pcr_target:a,pcr_target:b,geo_value:ca"`. Returns `NULL` for no
#' filters.
#' @keywords internal
.serialize_key_filters <- function(key_filters, max_vals = 10L) {
  if (!length(key_filters)) {
    return(NULL)
  }
  if (!rlang::is_named(key_filters)) {
    cli::cli_abort(
      "Every filter must be named, e.g. {.code pcr_target = \"sars-cov-2\"}.",
      class = "epidatr__epidata__unnamed_filter"
    )
  }
  over <- vapply(key_filters, length, integer(1)) > max_vals
  if (any(over)) {
    cli::cli_warn(
      "{.field {names(key_filters)[over]}} {?has/have} more than {max_vals} \\
       values; the request URL may be too long.",
      class = "epidatr__epidata__many_filtered_values"
    )
  }
  # One `key:value` term per value. `as.character` so a typed value (e.g. a Date)
  # serializes as "2024-01-01", not its integer day-count.
  terms <- unlist(mapply(
    function(k, vals) paste0(k, ":", as.character(vals)),
    names(key_filters), key_filters,
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
  paste(terms, collapse = ",")
}

#' @keywords internal
.cast_filter <- function(res, geo_values, reference_time, parsed_reference_times, report_time = NULL) {
  if (!inherits(res, "data.frame")) {
    return(res)
  }
  if (!identical(geo_values, "*")) {
    actual_geo_values <- tolower(trimws(unlist(strsplit(geo_values, ","))))
    res <- res[res$geo_value %in% actual_geo_values, ]
  }
  if (!identical(reference_time, "*")) {
    res <- filter_by_timeset(res, "reference_time", parsed_reference_times)
  }
  # EpiRange lower bound filter (upper bound handled by validate_version_query)
  if (inherits(report_time, "EpiRange") && "report_time" %in% names(res)) {
    res <- filter_by_timeset(res, "report_time", report_time)
  }
  res
}
