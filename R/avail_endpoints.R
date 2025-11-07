#' List all available Epidata API endpoints
#'
#' @description
#' Fetches a data frame of all Epidata API endpoints that can be accessed using
#' this package, with a brief description.
#'
#' @return A [`tibble::tibble`] of endpoints, with two columns:
#'   \item{Endpoint}{Name of the function for accessing this API endpoint.}
#'   \item{Description}{One-sentence description of the data available at the
#'   endpoint.}
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
  cli::cli_inform(c("i" = "Data is available for the US only, unless otherwise specified"))
  tib %>% print(n = 50)
}
