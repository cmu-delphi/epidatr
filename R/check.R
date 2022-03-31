check_string_param <- function(name, value, required = TRUE) {
  # string or string[]
  if ((required &&
    !(!is.null(value) &&
      is.character(value))) ||
    (!required && !(is.null(value) || is.character(value)))) {
    rlang::abort(paste0("argument ", name, " is not a string"),
      name = name, value = value, class = "invalid_argument"
    )
  }
}

check_int_param <- function(name, value, required = TRUE) {
  # numeric or numeric[]
  if ((required &&
    !(!is.null(value) &&
      is.numeric(value))) ||
    (!required && !(is.null(value) || is.numeric(value)))) {
    rlang::abort(paste0("argument ", name, " is not a number"),
      name = name, value = value, class = "invalid_argument"
    )
  }
}

is_epirange_like <- function(value) {
  # character or numeric or EpiRange or list(from=..., to=...)
  is.character(value) ||
    is.numeric(value) ||
    inherits(value, "EpiRange") ||
    (is.list(value) &&
      "from" %in% names(value) && "to" %in% names(value))
}

check_single_epirange_param <- function(name, value, required = TRUE) {
  if ((required &&
    !(!is.null(value) &&
      is_epirange_like(value))) ||
    (!required && !(is.null(value) || is_epirange_like(value)))) {
    rlang::abort(paste0("argument ", name, " is not a epirange"),
      name = name, value = value, class = "invalid_argument"
    )
  }
}

check_epirange_param <- function(name, value, required = TRUE) {
  if (is.null(value)) {
    if (required) {
      rlang::abort(paste0("argument ", name, " is not a epirange"),
        name = name, value = value, class = "invalid_argument"
      )
    }
    return()
  }
  if (is_epirange_like(value)) {
    return()
  }
  if (!is.list || !all(sapply(sapply(value, is_epirange_like)))) {
    rlang::abort(paste0("argument ", name, " is not a epirange"),
      name = name, value = value, class = "invalid_argument"
    )
  }
}

check_single_string_param <- function(name, value, required = TRUE) {
  if ((required &&
    !(!is.null(value) &&
      is.character(value) &&
      length(value) == 1)) ||
    (!required &&
      !(is.null(value) ||
        (
          is.character(value) && length(value) == 1
        )))) {
    rlang::abort(paste0("argument ", name, " is not a single string"),
      name = name, value = value, class = "invalid_argument"
    )
  }
}

check_single_int_param <- function(name, value, required = TRUE) {
  if ((required &&
    !(!is.null(value) &&
      is.numeric(value) &&
      length(value) == 1)) ||
    (!required &&
      !(is.null(value) || (is.numeric(value) && length(value) == 1)))) {
    rlang::abort(paste0("argument ", name, " is not a single integer"),
      name = name, value = value, class = "invalid_argument"
    )
  }
}
