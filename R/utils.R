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

#' @keywords internal
.cast_filter <- function(res, geo_values, time_values, parsed_time_values, version = NULL) {
  if (!inherits(res, "data.frame")) {
    return(res)
  }
  if (!identical(geo_values, "*")) {
    actual_geo_values <- tolower(trimws(unlist(strsplit(geo_values, ","))))
    res <- res[res$geo_value %in% actual_geo_values, ]
  }
  if (!identical(time_values, "*")) {
    res <- filter_by_timeset(res, "time_value", parsed_time_values)
  }
  # EpiRange lower bound filter (upper bound handled by validate_version_query)
  if (inherits(version, "EpiRange") && "version" %in% names(res)) {
    res <- filter_by_timeset(res, "version", version)
  }
  res
}
