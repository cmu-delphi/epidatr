

#'
#' builds a new EpiRange instances
#'
#' @param from start
#' @param to end
#' @return EpiRange instance
#'
#' @export
epirange <- function(from, to) {
  stopifnot((is.numeric(from) ||
               is.character(from)) && length(from) == 1)
  stopifnot((is.numeric(to) ||
               is.character(to)) && length(to) == 1)
  if (to < from) {
    t = from
    from = to
    to = t
  }
  structure(list(from = from, to = to), class = "EpiRange")
}

create_epidata_field_info <-
  function(name,
           type,
           description = '',
           categories = c()) {
    stopifnot(is.character(name) && length(name) == 1)
    stopifnot(
      is.character(type) &&
        length(type) == 1 &&
        type %in% c(
          'text',
          'int',
          'float',
          'date',
          'epiweek',
          'categorical',
          'bool'
        )
    )
    stopifnot(is.character(description && length(description) == 1))
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
