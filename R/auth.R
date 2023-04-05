#' @title Get the API key
#' @description Get the API key from getOption("delphi.epidata.key"), the environment variable DELPHI_EPIDATA_KEY, or from ~/.config/delphi_epidata/key
#' @return The API key
#' @export
get_auth_key <- function(verbose = FALSE) {
  key <- getOption("delphi.epidata.key")
  if (key != "") {
    if (verbose) print("Key found in delphi.epidata.key option")
    return(key)
  }

  key <- Sys.getenv("DELPHI_EPIDATA_KEY")
  if (key != "") {
    if (verbose) print("Key found in DELPHI_EPIDATA_KEY environment variable")
    return(key)
  }

  if (file.exists("~/.config/delphi_epidata/key")) {
    key <- readLines("~/.config/delphi_epidata/key")
    if (key != "") {
      if (verbose) print("Key found in ~/.config/delphi_epidata/key")
      return(key)
    }
  }

  warning("No key found. Please set one with set_auth_key() to avoid rate limits.")
  return("")
}

#' @title Set the API key
#' @description Set the API key in the environment variable DELPHI_EPIDATA_KEY and (optionally) in
#'   ~/.config/delphi_epidata/key
#' @export
set_auth_key <- function() {
  key <- readline("Enter your key: ")
  confirmation <- readline("Do you want to store the key in ~/.config/delphi_epidata/key? [y/n] ")
  if (confirmation != "y") {
    stop("Key not saved.")
  } else {
    dir.create("~/.config/delphi_epidata", showWarnings = FALSE, recursive = TRUE)
    print("Key saved to ~/.config/delphi_epidata/key")
    writeLines(key, "~/.config/delphi_epidata/key")
  }
  options(delphi.epidata.key = key)
}
