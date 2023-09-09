#' Getting the API key
#'
#' @description
#' Get the API key from the environment variable `DELPHI_EPIDATA_KEY` or
#' `getOption("delphi.epidata.key")`.
#'
#' @return The API key as a string or "".
#'
#' @export
get_auth_key <- function() {
  key <- Sys.getenv("DELPHI_EPIDATA_KEY", unset = "")
  if (key != "") {
    return(key)
  }

  key <- getOption("delphi.epidata.key", default = "")
  if (key != "") {
    return(key)
  }

  cli::cli_warn(
    c(
      "No API key found. You will be limited to non-complex queries and encounter rate limits if you proceed.
      To avoid this, you can get your key by registering
      at https://api.delphi.cmu.edu/epidata/admin/registration_form and then:",
      "i" = "set the environment variable DELPHI_EPIDATA_KEY",
      "i" = "set the option 'delphi.epidata.key'",
      "",
      "To save your key for later sessions (and hide it from your code), you can edit your .Renviron file with:",
      "i" = "usethis::edit_r_environ()"
    ),
    .frequency = "regularly",
    .frequency_id = "delphi.epidata.key"
  )
  return("")
}
