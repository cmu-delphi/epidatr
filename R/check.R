#' Allows string vectors of length 1
#' @importFrom checkmate assert_string
check_string_param <- function(name, value, required = TRUE) {
  null.ok <- !required
  assert_string(value, null.ok = null.ok, .var.name = name)
}

#' Allows string vectors of length > 1 and lists of string vectors of length 1
#' @importFrom checkmate assert_character
check_character_param <- function(name, value, required = TRUE) {
  null.ok <- !required
  assert_character(value, null.ok = null.ok, any.missing = FALSE, .var.name = name)
}

#' Allows a single numeric value
#' @importFrom checkmate assert_integerish
check_scalar_integerish_param <- function(name, value, required = TRUE) {
  null.ok <- !required
  assert_integerish(value, len = 1, null.ok = null.ok, .var.name = name)
}

#' Allows a vector of numeric values or a list of numeric values
#' @importFrom checkmate assert_integerish
check_integerish_param <- function(name, value, required = TRUE) {
  null.ok <- !required
  assert_integerish(value, null.ok = null.ok, any.missing = FALSE, .var.name = name)
}

#' Allows a scalar date_like param: a single date, character, or integerish
#' @importFrom checkmate assert check_date check_character check_integerish
check_scalar_date_param <- function(name, value, required = TRUE) {
  null.ok <- !required
  assert(
    # check_date(value, null.ok = null.ok, len = 1),
    check_character(value, null.ok = null.ok, len = 1),
    check_integerish(value, null.ok = null.ok, len = 1),
    combine = "or",
    .var.name = name
  )
}

#' Allows a vector of date_like params: dates, characters, or integerishes
#' @importFrom checkmate check_date check_character check_integerish
check_date_param <- function(name, value, required = TRUE) {
  null.ok <- !required
  assert(
    # check_date(value, null.ok = null.ok),
    check_character(value, null.ok = null.ok),
    check_integerish(value, null.ok = null.ok),
    combine = "or",
    .var.name = name
  )
}

#' Allows a scalar timeset param: a character, an integerish, or an EpiRange
#' @importFrom checkmate assert check_character check_integerish check_class
check_scalar_timeset_param <- function(name, value, required = TRUE) {
  null.ok <- !required
  assert(
    check_character(value, null.ok = null.ok, len = 1),
    check_integerish(value, null.ok = null.ok, len = 1),
    check_class(value, "EpiRange", null.ok = null.ok),
    combine = "or",
    .var.name = name
  )
}

#' Check if param is a character or numeric or EpiRange
#' @importFrom checkmate assert check_character check_integerish check_class check_list
check_timeset_param <- function(name, value, required = TRUE) {
  null.ok <- !required
  assert(
    check_character(value, null.ok = null.ok),
    check_integerish(value, null.ok = null.ok),
    check_class(value, "EpiRange", null.ok = null.ok),
    check_list(value, types = "EpiRange", null.ok = null.ok),
    combine = "or",
    .var.name = name
  )
}
