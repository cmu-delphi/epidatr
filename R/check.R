#' Allows character vectors
#' @importFrom checkmate assert_character assert_integerish
#' @keywords internal
assert_character_param <- function(name, value, len = NULL, required = TRUE) {
  null_ok <- !required
  assert_integerish(len, null.ok = TRUE, .var.name = "len")
  assert_character(value, null.ok = null_ok, len = len, any.missing = FALSE, .var.name = name)
}

#' Allows integer-like vectors
#' @importFrom checkmate assert_integerish
#' @keywords internal
assert_integerish_param <- function(name, value, len = NULL, required = TRUE) {
  null_ok <- !required
  assert_integerish(len, null.ok = TRUE, .var.name = "len")
  assert_integerish(value, null.ok = null_ok, len = len, any.missing = FALSE, .var.name = name)
}

#' Allows a vector of date_like params: date, character, or integer-like
#' @importFrom checkmate check_date check_character check_integerish
#' @keywords internal
assert_date_param <- function(name, value, len = NULL, required = TRUE) {
  null_ok <- !required
  assert_integerish(len, null.ok = TRUE, .var.name = "len")
  assert(
    check_date(value, len = len, any.missing = FALSE, null.ok = null_ok),
    check_character(value, len = len, any.missing = FALSE, null.ok = null_ok),
    check_integerish(value, len = len, any.missing = FALSE, null.ok = null_ok),
    combine = "or",
    .var.name = name
  )
}

#' Allows a timeset param: a date vector, a character vector, an integer-like
#' vector, or a single EpiRange
#' @importFrom checkmate assert check_character check_date check_integerish check_class check_list check_names
#' @keywords internal
assert_timeset_param <- function(name, value, len = NULL, required = TRUE) {
  null_ok <- !required
  assert_integerish(len, len = 1L, null.ok = TRUE, .var.name = "len")
  assert(
    check_class(value, "EpiRange", null.ok = null_ok),
    check_names(names(value), type = "unnamed"),
    combine = "or",
    .var.name = name
  )
  assert(
    check_date(value, len = len, any.missing = FALSE, null.ok = null_ok),
    check_character(value, len = len, any.missing = FALSE, null.ok = null_ok),
    check_integerish(value, len = len, any.missing = FALSE, null.ok = null_ok),
    check_class(value, "EpiRange", null.ok = null_ok),
    combine = "or",
    .var.name = name
  )
}
