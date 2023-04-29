#' Allows character vectors
#' @importFrom checkmate assert_character assert_integerish
assert_character_param <- function(name, value, len = NULL, required = TRUE) {
  null.ok <- !required
  assert_integerish(len, null.ok = TRUE, .var.name = "len")
  assert_character(value, null.ok = null.ok, len = len, any.missing = FALSE, .var.name = name)
}

#' Allows integer-like vectors
#' @importFrom checkmate assert_integerish
assert_integerish_param <- function(name, value, len = NULL, required = TRUE) {
  null.ok <- !required
  assert_integerish(len, null.ok = TRUE, .var.name = "len")
  assert_integerish(value, null.ok = null.ok, len = len, any.missing = FALSE, .var.name = name)
}

#' Allows a vector of date_like params: date, character, or integer-like
#' @importFrom checkmate check_date check_character check_integerish
assert_date_param <- function(name, value, len = NULL, required = TRUE) {
  null.ok <- !required
  assert_integerish(len, null.ok = TRUE, .var.name = "len")
  assert(
    check_date(value, len = len, any.missing = FALSE, null.ok = null.ok),
    check_character(value, len = len, any.missing = FALSE, null.ok = null.ok),
    check_integerish(value, len = len, any.missing = FALSE, null.ok = null.ok),
    combine = "or",
    .var.name = name
  )
}

#' Allows a vector of timeset params: date, character, integer-like, or EpiRange
#' @importFrom checkmate assert check_character check_date check_integerish check_class check_list
assert_timeset_param <- function(name, value, len = NULL, required = TRUE) {
  null.ok <- !required
  assert_integerish(len, null.ok = TRUE, .var.name = "len")
  assert(
    check_date(value, len = len, any.missing = FALSE, null.ok = null.ok),
    check_character(value, len = len, any.missing = FALSE, null.ok = null.ok),
    check_integerish(value, len = len, any.missing = FALSE, null.ok = null.ok),
    check_class(value, "EpiRange", null.ok = null.ok),
    check_list(value, len = len, types = "EpiRange", any.missing = FALSE, null.ok = null.ok),
    combine = "or",
    .var.name = name
  )
}
