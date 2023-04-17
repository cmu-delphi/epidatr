#' Get the API key
#' 
#' Get the API key from the environment variable DELPHI_EPIDATA_KEY or getOption("delphi.epidata.key")
#' 
#' @param verbose Will print the source of the key if TRUE (e.g. options, environment, or config file)
#' @return The API key
#' 
#' @export
get_auth_key <- function() {
  key <- Sys.getenv("DELPHI_EPIDATA_KEY", unset = "")
  if (key != "") return(key)

  key <- getOption("delphi.epidata.key", default = "")
  if (key != "") return(key)

  warning(
    paste0(
      "No API key found. To avoid being rate limited, you can:\n",
      " - set the environment variable DELPHI_EPIDATA_KEY.\n",
      " - set the option 'delphi.epidata.key' or\n",
      "\n",
      "To save your key for later sessions (and hide it from your code), you can edit your .Renviron file with:\n",
      "  usewith::edit_r_environ()"
    )
  )
  return("")
}
