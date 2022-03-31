parse_api_date <- function(value) {
  as.Date(as.character(value), format = "%Y%m%d")
}

#' parses a week
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
