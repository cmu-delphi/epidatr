
#'
#' says hello
#' @param name name to say hello to
#' @return string
#' @examples
#' hello()
#'
#' @export
hello <- function(name = 'World') {
    return(paste("Hello", name))
}
