#' List all available endpoints.
#'
#' @description
#' A function that prints a tibble with two columns: `Endpoint` contains the
#' function for accessing the Delphi Epidata API endpoint along with a
#' `Description`.
#'
#' @return A [`tibble::tibble`].
#' @export
#' @importFrom utils help.search
#'
#' @examples
#' avail_endpoints()
avail_endpoints <- function() {
  h <- help.search("endpoint",
    package = "epidatr", fields = "concept",
    agrep = FALSE
  )$matches
  tib <- tibble::tibble( # printing is much nicer than data.frame
    Endpoint = paste0(h$Name, "()"),
    Description = h$Title
  )
  tib %>% print(n = 50)
}
