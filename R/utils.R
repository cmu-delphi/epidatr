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

#' Warn that a V4 endpoint is being phased out in favor of the V5 API
#' @param fn_name string. Name of the calling function, used to throttle the
#'   warning and to point users at the right place in the migration guide.
#' @keywords internal
warn_v4_sunset <- function(fn_name) {
  cli::cli_warn(
    c(
      "{.fn {fn_name}} uses the V4 Epidata API.",
      "i" = "Starting in October 2026, V4 is tentatively deprecated in favor of the V5 API.",
      "i" = "See {.code vignette(\"migration-guide\")} (or {.url \\
       https://cmu-delphi.github.io/epidatr/articles/migration-guide.html}) \\
       for the V5 endpoints and how to move to them."
    ),
    .frequency = "regularly",
    .frequency_id = paste0("epidatr.v4_sunset.", fn_name)
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
  h <- help.search(
    "endpoint",
    package = "epidatr",
    fields = "concept",
    agrep = FALSE
  )$matches
  tib <- tibble::tibble(
    # printing is much nicer than data.frame
    Endpoint = paste0(h$Name, "()"),
    Description = h$Title
  )
  cli::cli_inform(c(
    "i" = "Data is available for the US only, unless otherwise specified"
  ))
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
    names(key_filters),
    key_filters,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ))
  paste(terms, collapse = ",")
}

#' @keywords internal
.cast_filter <- function(
  res,
  geo_values,
  reference_time,
  parsed_reference_times,
  report_time = NULL
) {
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

#' Diagnose an empty (or partially empty) cast-API result.
#'
#' On a partial result (some rows returned), warns about the signals/geo_types
#' that returned nothing, noting any that `epidata_meta()` says don't exist.
#' On a fully empty result, errors on an invalid `geo_type`/`signals`, warns
#' when the local `geo_values`/`reference_time` filters dropped every row the
#' server returned, and warns generically otherwise. No-op when
#' `fetch_args$return_empty` is `TRUE`.
#' @param result the filtered result (a data frame)
#' @param fetched the combined server response before local filtering
#' @param source,signals,geo_type the query parameters, for error/warning
#'   messages and for looking up `epidata_meta()`
#' @param fetch_args a `fetch_args` object
#' @keywords internal
.check_cast_empty <- function(
  result,
  fetched,
  source,
  signals,
  geo_type,
  fetch_args
) {
  if (isTRUE(fetch_args$return_empty)) {
    return(invisible(NULL))
  }

  empty_signals <- setdiff(signals, fetched[["signal"]])
  empty_geo_types <- if ("geo_type" %in% names(fetched)) {
    setdiff(geo_type, fetched[["geo_type"]])
  } else if (nrow(fetched) == 0) {
    geo_type
  } else {
    # Rows came back but carry no geo_type column: per-geo emptiness is unknowable.
    character()
  }
  meta <- NULL
  if (length(empty_signals) > 0 || length(empty_geo_types) > 0) {
    meta <- tryCatch(
      epidata_meta(source, fetch_args = fetch_args)[[source]],
      error = function(e) NULL
    )
  }
  bad_signals <- if (!is.null(meta)) {
    setdiff(empty_signals, meta$signals)
  } else {
    character()
  }
  bad_geo_types <- if (!is.null(meta)) {
    setdiff(empty_geo_types, meta$geo_types)
  } else {
    character()
  }

  total_rows <- nrow(result)
  server_total <- nrow(fetched)

  if (total_rows > 0) {
    # Partial result: warn about the empty parts, but never discard returned data.
    msg <- c()
    if (length(empty_signals) > 0) {
      msg <- c(
        msg,
        "!" = "No data returned for signal{?s} {.val {empty_signals}}."
      )
    }
    if (length(empty_geo_types) > 0) {
      msg <- c(
        msg,
        "!" = "No data returned for geo_type{?s} {.val {empty_geo_types}}."
      )
    }
    if (length(bad_signals) > 0) {
      msg <- c(
        msg,
        "x" = "{.val {bad_signals}} {?is/are} not {?an /}available signal{?s} for \\
        source {.val {source}}. Available signals: {.val {meta$signals}}."
      )
    }
    if (length(bad_geo_types) > 0) {
      msg <- c(
        msg,
        "x" = "{.val {bad_geo_types}} {?is/are} not {?an /}available geo_type{?s} for \\
        source {.val {source}}. Available geo_types: {.val {meta$geo_types}}."
      )
    }
    if (length(msg) > 0) {
      cli::cli_warn(msg, class = "epidatr__empty_signals")
    }
    return(invisible(NULL))
  }

  # From here, total_rows == 0: nothing to salvage, so invalid keys are an error.
  if (length(bad_geo_types) > 0) {
    cli::cli_abort(
      "{.val {bad_geo_types}} {?is/are} not {?an /}available geo_type{?s} for source {.val {source}}. \\
       Available geo_types: {.val {meta$geo_types}}.",
      class = "epidatr__epidata__invalid_geo_type"
    )
  }
  if (length(bad_signals) > 0) {
    cli::cli_abort(
      "{.val {bad_signals}} {?is/are} not {?an /}available signal{?s} for source {.val {source}}. \\
       Available signals: {.val {meta$signals}}.",
      class = "epidatr__epidata__invalid_signals"
    )
  }

  if (server_total > 0) {
    cli::cli_warn(
      "The API returned {server_total} row{?s} total, but the local {.field geo_values}/\\
       {.field reference_time} filters matched none of them.",
      class = "epidatr__empty_result"
    )
    return(invisible(NULL))
  }

  msg <- c("Query returned no rows.")
  if (!is.null(meta) && !is.null(meta$reference_time_range)) {
    msg <- c(
      msg,
      "i" = "Source {.val {source}}'s reference_time range: \\
      {meta$reference_time_range$first} to {meta$reference_time_range$latest}."
    )
  }
  cli::cli_warn(msg, class = "epidatr__empty_result")
  invisible(NULL)
}
