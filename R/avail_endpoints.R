#' List all available endpoints
#'
#' @return A [`tibble::tibble`] with 2 columns. `Endpoint` contains the function
#'   for accessing the Delphi Epidata API endpoint along with a `Description`.
#' @export
#' @importFrom utils help.search
#'
#' @examples
#' endps <- avail_endpoints()
#' print(endps, n = nrow(endps))
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
