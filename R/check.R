check_string_param <- function(name, value, required=TRUE) {
  # string or string[]
  if ((required && !(!is.null(value) && is.character(value))) || (!required && !(is.null(value) || is.character(value)))) {
    stop(paste0("argument ", name, " is not a string"))
  }
}

is_epirange_like <- function(value) {
  is.character(value) || is.numeric(value) || inherits(value, 'EpiRange') || (is.list(value) && 'from' %in% names(value) && 'to' %in% names(value))
}

check_single_epirange_param <- function(name, value, required=TRUE) {
  if ((required && !(!is.null(value) && is_epirange_like(value))) || (!required && !(is.null(value) || is_epirange_like(value)))) {
    stop(paste0("argument ", name, " is not a epirange"))
  }
}

check_epirange_param <- function(name, value, required=TRUE) {
  if (is.null(value)) {
    if (required) {
      stop(paste0("argument ", name, " is not a epirange"))
    }
    return()
  }
  if (is_epirange_like(value)) {
    return()
  }
  if (!is.list || !all(sapply(sapply(values, is_epirange_like)))) {
    stop(paste0("argument ", name, " is not a epirange"))
  }
}

check_single_string_param <- function(name, value, required=TRUE) {
  if ((required && !(!is.null(value) && is.character(value) && length(value) == 1)) || (!required && !(is.null(value) || (is.character(value) && length(value) == 1)))) {
    stop(paste0("argument ", name, " is not a single string"))
  }
}

check_single_int_param <- function(name, value, required=TRUE) {
  if ((required && !(!is.null(value) && is.numeric(value) && length(value) == 1)) || (!required && !(is.null(value) || (is.numeric(value) && length(value) == 1)))) {
    stop(paste0("argument ", name, " is not a single integer"))
  }
}
