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

  if (inherits(from, "Date")) {
    from <- as.numeric(format(from, "%Y%m%d"))
  }
  if (inherits(to, "Date")) {
    to <- as.numeric(format(to, "%Y%m%d"))
  }

  if (to < from) {
    t <- from
    from <- to
    to <- t
  }

  structure(list(from = from, to = to), class = "EpiRange")
}

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
