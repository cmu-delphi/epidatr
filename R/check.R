#' Allows string vectors of length 1
check_single_string_param <- function(name, value, required = TRUE) {
  if (is.null(value)) {
    if (required) {
      rlang::abort(paste0("argument ", name, " is not a single string"),
        name = name, value = value, class = "invalid_argument"
      )
    }
  } else if (is.character(value)) {
    if (length(value) != 1) {
      rlang::abort(paste0("argument ", name, " is not a single string"),
        name = name, value = value, class = "invalid_argument"
      )
    }
  } else {
    rlang::abort(paste0("argument ", name, " is not a single string"),
      name = name, value = value, class = "invalid_argument"
    )
  }
}

#' Allows string vectors of length > 1 and lists of string vectors of length 1
check_string_param <- function(name, value, required = TRUE) {
  if (is.null(value)) {
    if (required) {
      rlang::abort(paste0("argument ", name, " is not a string"),
        name = name, value = value, class = "invalid_argument"
      )
    }
  } else if (is.character(value)) {
    return()
  } else if (is.list(value)) {
    for (i in seq_along(value)) {
      if (!is.character(value[[i]]) || length(value[[i]]) != 1) {
        rlang::abort(paste0("argument ", name, "[", i, "] is not a single string"),
          name = name, value = value, class = "invalid_argument"
        )
      }
    }
  } else {
    rlang::abort(paste0("argument ", name, " is not a string"),
      name = name, value = value, class = "invalid_argument"
    )
  }
}

#' Allows a single numeric value
check_single_int_param <- function(name, value, required = TRUE) {
  if (is.null(value)) {
    if (required) {
      rlang::abort(paste0("argument ", name, " is not a single number"),
        name = name, value = value, class = "invalid_argument"
      )
    }
  } else if (is.numeric(value)) {
    if (length(value) != 1) {
      rlang::abort(paste0("argument ", name, " is not a single number"),
        name = name, value = value, class = "invalid_argument"
      )
    }
  } else {
    rlang::abort(paste0("argument ", name, " is not a single number"),
      name = name, value = value, class = "invalid_argument"
    )
  }
}

#' Allows a vector of numeric values or a list of numeric values
check_int_param <- function(name, value, required = TRUE) {
  if (is.null(value)) {
    if (required) {
      rlang::abort(paste0("argument ", name, " is not a number"),
        name = name, value = value, class = "invalid_argument"
      )
    }
  } else if (is.numeric(value)) {
    return()
  } else if (is.list(value)) {
    for (i in seq_along(value)) {
      if (!is.numeric(value[[i]]) || length(value[[i]]) != 1) {
        rlang::abort(paste0("argument ", name, "[", i, "] is not a single number"),
          name = name, value = value, class = "invalid_argument"
        )
      }
    }
  } else {
    rlang::abort(paste0("argument ", name, " is not a number"),
      name = name, value = value, class = "invalid_argument"
    )
  }
}

#' Return TRUE if value is datelike
#' TODO: add Date support
is_date_like <- function(value) {
  is.character(value) ||
    is.numeric(value)
}

#' Check if param is datelike
check_single_date_param <- function(name, value, required = TRUE) {
  if (is.null(value)) {
    if (required) {
      rlang::abort(paste0("argument ", name, " is not a datelike"),
        name = name, value = value, class = "invalid_argument"
      )
    }
  } else if (is_date_like(value) && length(value) == 1) {
    return()
  } else {
    rlang::abort(paste0("argument ", name, " is not a datelike"),
      name = name, value = value, class = "invalid_argument"
    )
  }
}

#' Check if param is datelike or vector of datelike
check_date_param <- function(name, value, required = TRUE) {
  if (is.null(value)) {
    if (required) {
      rlang::abort(paste0("argument ", name, " is not a datelike"),
        name = name, value = value, class = "invalid_argument"
      )
    }
  } else if (is_date_like(value)) {
    return()
  } else if (is.list(value) && length(value) > 0) {
    for (i in seq_along(value)) {
      check_single_date_param(paste0(name, "[", i, "]"), value[[i]], required = TRUE)
    }
  } else {
    rlang::abort(paste0("argument ", name, " is not a datelike"),
      name = name, value = value, class = "invalid_argument"
    )
  }
}

#' Return TRUE if value is a character or numeric or EpiRange
#' TODO: add Date support
is_epirange_like <- function(value) {
  is.character(value) ||
    is.numeric(value) ||
    inherits(value, "EpiRange")
}

#' Check if param is a character or numeric or EpiRange
check_single_epirange_param <- function(name, value, required = TRUE) {
  if (is.null(value)) {
    if (required) {
      rlang::abort(paste0("argument ", name, " is not an epirange_like"),
        name = name, value = value, class = "invalid_argument"
      )
    }
  } else if (is_epirange_like(value)) {
    return()
  } else {
    rlang::abort(paste0("argument ", name, " is not an epirange_like"),
      name = name, value = value, class = "invalid_argument"
    )
  }
}

#' Check if param is a character or numeric or EpiRange
check_epirange_param <- function(name, value, required = TRUE) {
  if (is.null(value)) {
    if (required) {
      rlang::abort(paste0("argument ", name, " is not an epirange_like"),
        name = name, value = value, class = "invalid_argument"
      )
    }
    return()
  } else if (is_epirange_like(value)) {
    return()
  } else if (all(map(value, typeof) == "list") && length(value) > 0) {
    for (i in seq_along(value)) {
      check_single_epirange_param(paste0(name, "[", i, "]"), value[[i]], required = TRUE)
    }
  } else if (sum(names(value) %in% c("from", "to")) >= 2) {
    rlang::abort(paste0("argument ", name, " does not accept vectors of epirange_likes, please use a list instead"),
      name = name, value = value, class = "invalid_argument"
    )
  } else {
    rlang::abort(paste0("argument ", name, " is not an epirange_like"),
      name = name, value = value, class = "invalid_argument"
    )
  }
}
