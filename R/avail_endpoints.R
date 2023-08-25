#' List all available endpoints
#'
#' @return A [`tibble::tibble`] with 2 columns. `Endpoint` contains the function
#'   for accessing the Delphi Epidata API endpoint along with a `Description`.
#' @export
#'
#' @examples
#' endps <- avail_endpoints()
#' print(endps, n = nrow(endps))
avail_endpoints <- function() {
  db <- utils::hsearch_db("epidatr")
  endpoint_ids <- db$Concepts$ID[db$Concepts$Concept == "endpoint"]
  all_ids <- db$Base$ID
  our_endpoints <- all_ids %in% endpoint_ids
  tib <- tibble::tibble( # printing is much nicer than data.frame
    Endpoint = paste0(db$Base$Name[our_endpoints], "()"),
    Description = db$Base$Title[our_endpoints]
  )
  tib
}
