
#'
#' builds a new EpiRange instances
#'
#' @param from start
#' @param to end
#' @return EpiRange instance
#'
#' @export
epirange <- function(from, to) {
    stopifnot((is.numeric(from) || is.character(from)) && length(from) == 1)
    stopifnot((is.numeric(to) || is.character(to)) && length(to) == 1)
    if(to < from) {
        t = from
        from = to
        to = t
    }
    structure(list(from=from, to=to), class="EpiRange")
}
